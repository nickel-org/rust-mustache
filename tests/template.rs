use std::cell::RefCell;
use tempfile::Builder;
use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::path::{PathBuf, Path};
use std::collections::HashMap;

use mustache::{self, Data, Error, to_data};
use mustache::{Context, Template};

use serde::Serialize;
use serde_json;
use serde_json::Value as Json;

#[derive(Debug, Serialize)]
struct Planet {
    name: String,
    info: Option<PlanetInfo>,
}

#[derive(Debug, Serialize)]
struct PlanetInfo {
    moons: Vec<String>,
    population: u64,
    description: String,
}

#[derive(Debug, Serialize)]
struct Person {
    name: String,
    age: Option<u32>,
}

fn compile_str(s: &str) -> Template {
    mustache::compile_str(s).expect(&format!("Failed to compile: {}", s))
}

fn assert_render<T>(template: &str, data: &T) -> String
where T: Serialize + Debug
{
    if let Ok(s) = render(template, data) {
        s
    } else {
        panic!("Failed to render: template: {:?}\ndata: {:?}", template, data);
    }
}

fn render<T>(template: &str, data: &T) -> Result<String, Error>
where T: Serialize
{
    let template = compile_str(template);

    let mut bytes = vec![];
    template.render(&mut bytes, data)?;

    Ok(String::from_utf8(bytes).expect("Failed to encode String"))
}

#[test]
fn test_render_texts() {
    let ctx = Planet {
        name: "world".to_string(),
        info: None,
    };

    assert_eq!(assert_render("hello world", &ctx), "hello world");
    assert_eq!(assert_render("hello {world", &ctx), "hello {world");
    assert_eq!(assert_render("hello world}", &ctx), "hello world}");
    assert_eq!(assert_render("hello {world}", &ctx), "hello {world}");
    assert_eq!(assert_render("hello world}}", &ctx), "hello world}}");
}

#[test]
fn test_render_etags() {
    let ctx = Planet {
        name: "world".to_string(),
        info: None,
    };

    assert_eq!(assert_render("hello {{name}}", &ctx), "hello world");
}

#[test]
fn test_render_utags() {
    let ctx = Planet {
        name: "world".to_string(),
        info: None,
    };

    assert_eq!(assert_render("hello {{{name}}}", &ctx), "hello world");

    assert_eq!(assert_render("hello {{{name}}}", &ctx), "hello world");
}

#[test]
fn test_render_option() {
    let template = "{{name}}, {{age}}";
    let ctx = Person {
        name: "Dennis".to_string(),
        age: None,
    };

    assert_eq!(assert_render(template, &ctx), "Dennis, ");

    let ctx = Person {
        name: "Dennis".to_string(),
        age: Some(42),
    };
    assert_eq!(assert_render(template, &ctx), "Dennis, 42");
}

#[test]
fn test_render_option_sections_implicit() {
    let template = "{{name}}, {{#age}}{{.}}{{/age}}{{^age}}No age{{/age}}";
    let ctx = Person {
        name: "Dennis".to_string(),
        age: None,
    };

    assert_eq!(assert_render(template, &ctx), "Dennis, No age");

    let ctx = Person {
        name: "Dennis".to_string(),
        age: Some(42),
    };
    assert_eq!(assert_render(template, &ctx), "Dennis, 42");
}

#[test]
fn test_implicit_section() {
    let template = compile_str("{{#.}}{{.}}{{/.}}");
    let data = vec!["val1", "val2"];
    let mut r = Vec::new();
    template.render(&mut r, &data).unwrap();
    assert_eq!(r, b"val1val2");
}

#[test]
fn test_implicit_inverted_section() {
    let template = compile_str("{{^.}}No values{{/.}}");
    let data = Vec::<&str>::new();
    let mut r = Vec::new();
    template.render(&mut r, &data).unwrap();
    assert_eq!(r, b"No values");
}

#[test]
fn test_nested_implicit_render() {
    let template = compile_str("{{#.}}[{{#.}}{{.}}{{/.}}]{{/.}}");
    let data = vec![vec!["val1", "val2"], vec!["val3"]];
    let mut r = Vec::new();
    template.render(&mut r, &data).unwrap();
    assert_eq!(r, b"[val1val2][val3]");
}

fn test_implicit_render(tags: &str, expect_escaped: bool) {
    let template = format!("{}{}{}", "{{#list}} (", tags, ") {{/list}}");
    let mut ctx = HashMap::new();
    ctx.insert("list", vec!["&", "\"", "<", ">"]);

    let expected = if expect_escaped {
        " (&amp;)  (&quot;)  (&lt;)  (&gt;) "
    } else {
        r#" (&)  (")  (<)  (>) "#
    };

    assert_eq!(&*render(&template, &ctx).unwrap(), expected);
}

#[test]
fn test_render_option_sections_implicit_escaped() {
    test_implicit_render("{{.}}", true);
}

#[test]
fn test_render_option_sections_implicit_escaped_alternative_delimeters() {
    test_implicit_render("{{=<% %>=}}<%.%><%={{ }}=%>", true);
}

#[test]
fn test_render_option_sections_implicit_unescaped() {
    test_implicit_render("{{{.}}}", false);
}

#[test]
fn test_render_option_sections_implicit_ampersand() {
    test_implicit_render("{{&.}}", false);
}

#[test]
fn test_render_option_sections_implicit_unescaped_alternative_delimeters() {
    test_implicit_render("{{=<% %>=}}<%{.}%><%={{ }}=%>", false);
}

#[test]
fn test_render_option_sections_implicit_ampersand_alternative_delimeters() {
    test_implicit_render("{{=<% %>=}}<%&.%><%={{ }}=%>", false);
}

mod context_search {
    use super::{render_data, compile_str};
    use mustache::MapBuilder;

    fn assert_render(template: &str, expected: &str) {
        let ctx = MapBuilder::new().insert_map("payload", |map| {
            map.insert_vec("list", |vec| {
                vec.push_str("<p>hello</p>")
                   .push_str("<p>world</p>")
            }).insert_str("test", "hello world")
        }).build();

        let template = compile_str(template);
        let rendered = render_data(&template, &ctx);
        println!("{}\n----\n{}", rendered, expected);
        assert_eq!(rendered, expected);
    }

    #[test]
    fn renders_bool() {
        let template = compile_str(
"{{bool}}
#map{{#outer}}
    {{#bool}}
        #bool
        #vec{{#inner}}{{{.}}}{{/inner}}/vec
    {{/bool}}\n\
    {{^not_ok}}
        ^not_ok
        #vec{{#inner}}{{{.}}}{{/inner}}/vec
    {{/not_ok}}\n\
{{/outer}}
/map
{{ok}}");
        let ctx = MapBuilder::new()
            .insert_bool("bool", false)
            .insert_bool("not_ok", false)
            .insert_map("outer", |map| {
                map.insert_bool("bool", true)
                    .insert_vec("inner", |vec| {
                        vec.push_bool(false)
                            .push_bool(true)
                            .push_bool(false)
                    })
            })
            .insert_bool("ok", true)
            .build();

        let expected = "false
#map
        #bool
        #vecfalsetruefalse/vec

        ^not_ok
        #vecfalsetruefalse/vec

/map
true";
        let rendered = render_data(&template, &ctx);
        println!("{}\n----\n{}", rendered, expected);
        assert_eq!(rendered, expected);
    }

    #[test]
    fn from_base() {
        let template = "\
{{#payload.list}}
{{payload.test}}
{{/payload.list}}";

        assert_render(template, "hello world\nhello world\n")
    }

    #[test]
    fn from_mid_stack() {
        let template = "\
{{#payload}}
{{#list}}
{{test}}
{{/list}}
{{/payload}}";

        assert_render(template, "hello world\nhello world\n")
    }

    #[test]
    fn render_nothing_when_not_found() {
        let template = "\
{{#payload.list}}
{{test}}
{{/payload.list}}";

        assert_render(template, "\n\n")
    }
}

#[test]
fn test_render_option_nested() {
    #[derive(Debug, Serialize)]
    struct Nested {
        opt: Option<Option<u32>>,
    }

    let template = "-{{opt}}+";
    let ctx = Nested { opt: None };
    assert_eq!(assert_render(template, &ctx), "-+");

    let ctx = Nested { opt: Some(None) };
    assert_eq!(assert_render(template, &ctx), "-+");

    let ctx = Nested { opt: Some(Some(42)) };
    assert_eq!(assert_render(template, &ctx), "-42+");
}

#[test]
fn test_render_option_complex() {
    let template = "{{name}} - {{#info}}{{description}}; It's moons are \
                    [{{#moons}}{{.}}{{/moons}}] and has a population of \
                    {{population}}{{/info}}{{^info}}No additional info{{/info}}";
    let ctx = Planet {
        name: "Jupiter".to_string(),
        info: None,
    };
    assert_eq!(assert_render(template, &ctx), "Jupiter - No additional info");

    let address = PlanetInfo {
        moons: vec!["Luna".to_string()],
        population: 7300000000,
        description: "Birthplace of rust-lang".to_string(),
    };
    let ctx = Planet {
        name: "Earth".to_string(),
        info: Some(address),
    };
    assert_eq!(assert_render(template, &ctx), "Earth - Birthplace of rust-lang; \
                                               It's moons are [Luna] and has a \
                                               population of 7300000000");
}

fn render_data(template: &Template, data: &Data) -> String {
    let mut bytes = vec![];
    template.render_data(&mut bytes, data).expect("Failed to render data");
    String::from_utf8(bytes).expect("Failed ot encode as String")
}

#[test]
fn test_write_failure() {
    let mut ctx = HashMap::new();
    ctx.insert("name", "foobar");

    let template = compile_str("{{name}}");
    let mut buffer = [0u8; 6];
    {
        let mut writer: &mut [u8] = &mut buffer;
        template.render(&mut writer, &ctx).expect("Failed to render");
    }

    assert_eq!(&buffer, b"foobar");

    ctx.insert("name", "longerthansix");

    let mut writer: &mut [u8] = &mut buffer;
    assert!(template.render(&mut writer, &ctx).is_err());
    //assert_let!(Err(e) = template.render(&mut writer, &ctx) => {
    //    assert_eq!(format!("{}", e.to_string()), "failed to write whole buffer")
    //})
}

#[test]
fn test_render_to_string() {
    let mut ctx = HashMap::new();
    ctx.insert("name", "foobar");

    let template = compile_str("{{name}}");
    let result = template.render_to_string(&ctx).expect("Failed to render");

    assert_eq!(&result, "foobar");
}

#[test]
fn test_render_data_to_string() {
    let ctx = HashMap::new();
    let template = compile_str("0{{#a}}1 {{n}} 3{{/a}}5");

    let result = template.render_data_to_string(&Data::Map(ctx)).expect("Failed to render");

    assert_eq!(&result, "05");

    let mut ctx0 = HashMap::new();
    let mut ctx1 = HashMap::new();
    ctx1.insert("n".to_string(), Data::String("a".to_string()));
    ctx0.insert("a".to_string(), Data::Vec(vec![Data::Map(ctx1)]));

    let result = template.render_data_to_string(&Data::Map(ctx0)).expect("Failed to render");

    assert_eq!(&result, "01 a 35");
}

#[test]
fn test_render_sections() {
    let ctx = HashMap::new();
    let template = compile_str("0{{#a}}1 {{n}} 3{{/a}}5");

    assert_eq!(render_data(&template, &Data::Map(ctx)), "05".to_string());

    let mut ctx = HashMap::new();
    ctx.insert("a".to_string(), Data::Vec(Vec::new()));

    assert_eq!(render_data(&template, &Data::Map(ctx)), "05".to_string());

    let mut ctx = HashMap::new();
    ctx.insert("a".to_string(), Data::Vec(Vec::new()));
    assert_eq!(render_data(&template, &Data::Map(ctx)), "05".to_string());

    let mut ctx0 = HashMap::new();
    let ctx1 = HashMap::new();
    ctx0.insert("a".to_string(), Data::Vec(vec![Data::Map(ctx1)]));

    assert_eq!(render_data(&template, &Data::Map(ctx0)), "01  35".to_string());

    let mut ctx0 = HashMap::new();
    let mut ctx1 = HashMap::new();
    ctx1.insert("n".to_string(), Data::String("a".to_string()));
    ctx0.insert("a".to_string(), Data::Vec(vec![Data::Map(ctx1)]));
    assert_eq!(render_data(&template, &Data::Map(ctx0)), "01 a 35".to_string());

    let mut ctx = HashMap::new();
    ctx.insert("a".to_string(),
               Data::Fun(RefCell::new(Box::new(|_text| "foo".to_string()))));
    assert_eq!(render_data(&template, &Data::Map(ctx)), "0foo5".to_string());
}

#[test]
fn test_render_inverted_sections() {
    let template = compile_str("0{{^a}}1 3{{/a}}5");

    let ctx = HashMap::new();
    assert_eq!(render_data(&template, &Data::Map(ctx)), "01 35".to_string());

    let mut ctx = HashMap::new();
    ctx.insert("a".to_string(), Data::Vec(vec![]));
    assert_eq!(render_data(&template, &Data::Map(ctx)), "01 35".to_string());

    let mut ctx0 = HashMap::new();
    let ctx1 = HashMap::new();
    ctx0.insert("a".to_string(), Data::Vec(vec![Data::Map(ctx1)]));
    assert_eq!(render_data(&template, &Data::Map(ctx0)), "05".to_string());

    let mut ctx0 = HashMap::new();
    let mut ctx1 = HashMap::new();
    ctx1.insert("n".to_string(), Data::String("a".to_string()));
    ctx0.insert("a".to_string(), Data::Vec(vec![Data::Map(ctx1)]));
    assert_eq!(render_data(&template, &Data::Map(ctx0)), "05".to_string());
}

fn assert_partials_data(template: Template) {
    let ctx = HashMap::new();
    assert_eq!(render_data(&template, &Data::Map(ctx)),
               "<h2>Names</h2>\n".to_string());

    let mut ctx = HashMap::new();
    ctx.insert("names".to_string(), Data::Vec(vec![]));
    assert_eq!(render_data(&template, &Data::Map(ctx)),
               "<h2>Names</h2>\n".to_string());

    let mut ctx0 = HashMap::new();
    let ctx1 = HashMap::new();
    ctx0.insert("names".to_string(), Data::Vec(vec![Data::Map(ctx1)]));
    assert_eq!(render_data(&template, &Data::Map(ctx0)),
               "<h2>Names</h2>\n  <strong></strong>\n\n".to_string());

    let mut ctx0 = HashMap::new();
    let mut ctx1 = HashMap::new();
    ctx1.insert("name".to_string(), Data::String("a".to_string()));
    ctx0.insert("names".to_string(), Data::Vec(vec![Data::Map(ctx1)]));
    assert_eq!(render_data(&template, &Data::Map(ctx0)),
               "<h2>Names</h2>\n  <strong>a</strong>\n\n".to_string());

    let mut ctx0 = HashMap::new();
    let mut ctx1 = HashMap::new();
    ctx1.insert("name".to_string(), Data::String("a".to_string()));
    let mut ctx2 = HashMap::new();
    ctx2.insert("name".to_string(), Data::String("<b>".to_string()));
    ctx0.insert("names".to_string(), Data::Vec(vec![Data::Map(ctx1), Data::Map(ctx2)]));
    assert_eq!(render_data(&template, &Data::Map(ctx0)),
               "<h2>Names</h2>\n  <strong>a</strong>\n\n  <strong>&lt;b&gt;</strong>\n\n"
                   .to_string());
}

#[test]
fn test_render_partial_dot_filename() {
    let template = mustache::compile_path("tests/test-data/base.foo.mustache").expect("Failed to compile");
    assert_partials_data(template);
}

#[test]
fn test_render_partial() {
    let template = mustache::compile_path("tests/test-data/base").expect("Failed to compile");
    assert_partials_data(template);
}

fn parse_spec_tests(src: &str) -> Vec<Json> {
    let path = PathBuf::from(src);
    let file = File::open(&path)
        .expect(&format!("Could not read file {}", path.display()));
    let json = serde_json::from_reader(file)
        .expect(&format!("Invalid json in file {}", path.display()));

    assert_let!(Json::Object(mut d) = json => {
        assert_let!(Some(Json::Array(tests)) = d.remove("tests") => {
            tests.into_iter().collect()
        })
    })
}

fn write_partials(tmpdir: &Path, value: &Json) {
    assert_let!(Json::Object(ref d) = *value => {
        for (key, value) in d {
            assert_let!(Json::String(ref s) = *value => {
                let path = tmpdir.join(&(key.clone() + ".mustache"));
                File::create(&path)
                    .and_then(|mut f| f.write_all(s.as_bytes()))
                    .expect("Failed to generate partial");
            })
        }
    })
}

fn run_test(test: serde_json::Map<String, Json>, data: Data) {
    let template = assert_let!(Some(&Json::String(ref s)) = test.get("template") => {
        s.clone()
    });

    let expected = assert_let!(Some(&Json::String(ref s)) = test.get("expected") => {
        s.clone()
    });

    // Make a temporary dir where we'll store our partials. This is to
    // avoid a race on filenames.
    let tmpdir = Builder::new().tempdir().expect("Failed to make tempdir");

    if let Some(value) = test.get("partials") {
        write_partials(tmpdir.path(), value)
    }

    let ctx = Context::new(tmpdir.path().to_path_buf());
    let template = ctx.compile(template.chars())
                      .expect(&format!("Failed to compile: {}", template));
    let result = render_data(&template, &data);

    if result != expected {
        println!("desc:     {:?}", test.get("desc"));
        println!("context:  {:?}", test.get("data"));
        println!("=>");
        println!("template: {:?}", template);
        println!("expected: {}", expected);
        println!("actual:   {}", result);
        println!("");
    }
    assert_eq!(result, expected);
}

fn run_tests(spec: &str) {
    for json in parse_spec_tests(spec).into_iter() {
        let test = assert_let!(Json::Object(m) = json => m);

        let data = test.get("data").expect("No test data").clone();
        let data = to_data(&data).expect("Failed to encode");

        run_test(test, data);
    }
}

#[test]
fn test_spec_comments() {
    run_tests("spec/specs/comments.json");
}

#[test]
fn test_spec_delimiters() {
    run_tests("spec/specs/delimiters.json");
}

#[test]
fn test_spec_interpolation() {
    run_tests("spec/specs/interpolation.json");
}

#[test]
fn test_spec_inverted() {
    run_tests("spec/specs/inverted.json");
}

#[test]
fn test_spec_partials() {
    run_tests("spec/specs/partials.json");
}

#[test]
fn test_spec_sections() {
    run_tests("spec/specs/sections.json");
}

#[test]
fn test_spec_lambdas() {
    for json in parse_spec_tests("spec/specs/~lambdas.json").into_iter() {
        let mut test = assert_let!(Json::Object(m) = json => m);

        let s = assert_let!(Some(Json::String(s)) = test.remove("name") => s);

        // Replace the lambda with rust code.
        let data = test.remove("data").expect("No test data");

        let data = to_data(&data).expect("Failed to encode");

        let mut ctx = assert_let!(Data::Map(ctx) = data => ctx);

        // needed for the closure test.
        let mut calls = 0usize;

        match &*s {
            "Interpolation" => {
                let f = |_text| "world".to_string();
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            "Interpolation - Expansion" => {
                let f = |_text| "{{planet}}".to_string();
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            "Interpolation - Alternate Delimiters" => {
                let f = |_text| "|planet| => {{planet}}".to_string();
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            "Interpolation - Multiple Calls" => {
                let f = move |_text: String| {
                    calls += 1;
                    calls.to_string()
                };
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            "Escaping" => {
                let f = |_text| ">".to_string();
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            "Section" => {
                let f = |text: String| {
                    if text == "{{x}}" {
                        "yes".to_string()
                    } else {
                        "no".to_string()
                    }
                };
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            "Section - Expansion" => {
                let f = |text: String| text.clone() + "{{planet}}" + &text;
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            "Section - Alternate Delimiters" => {
                let f = |text: String| text.clone() + "{{planet}} => |planet|" + &text;
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            "Section - Multiple Calls" => {
                let f = |text: String| "__".to_string() + &text + "__";
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            "Inverted Section" => {
                let f = |_text| "".to_string();
                ctx.insert("lambda".to_string(), Data::Fun(RefCell::new(Box::new(f))));
            }
            spec_name => panic!("unimplemented lambda spec test: {}", spec_name),
        };

        run_test(test, Data::Map(ctx));
    }
}
