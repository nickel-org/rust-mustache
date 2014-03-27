#[feature(phase)];
#[phase(syntax, link)] extern crate log;

extern crate mustache;
extern crate serialize;
extern crate collections;

#[cfg(test)]
mod test {
    use std::str;
    use collections::hashmap::HashMap;
    use std::io::{File, TempDir};
    use serialize::json;
    use serialize::Encodable;
    use mustache::{compile_str, render_str};
    use mustache::{Context};
    use mustache::encoder::{Encoder, Data, Str, Vec, Map};
    use mustache::parser::{Token, Text, ETag, UTag, Section, IncompleteSection, Partial};

    fn token_to_str(token: &Token) -> ~str {
        match *token {
            // recursive enums crash %?
            Section(ref name,
                    inverted,
                    ref children,
                    ref otag,
                    ref osection,
                    ref src,
                    ref tag,
                    ref ctag) => {
                let name = name.iter().map(|e| format!("{:?}", *e)).collect::<Vec<~str>>();
                let children = children.iter().map(|x| token_to_str(x)).collect::<Vec<~str>>();
                format!("Section(vec!({}), {:?}, vec!({}), {:?}, {:?}, {:?}, {:?}, {:?})",
                        name.connect(", "),
                        inverted,
                        children.connect(", "),
                        otag,
                        osection,
                        src,
                        tag,
                        ctag)
            }
            ETag(ref name, ref tag) => {
                let name = name.iter().map(|e| format!("{:?}", *e)).collect::<Vec<~str>>();
                format!("ETag(vec!({:?}), {:?})", name.connect(", "), *tag)
            }
            UTag(ref name, ref tag) => {
                let name = name.iter().map(|e| format!("{:?}", *e)).collect::<Vec<~str>>();
                format!("UTag(vec!({:?}), {:?})", name.connect(", "), *tag)
            }
            IncompleteSection(ref name, ref inverted, ref osection, ref newlined) => {
                let name = name.iter().map(|e| format!("{:?}", *e)).collect::<Vec<~str>>();
                format!("IncompleteSection(vec!({:?}), {:?}, {:?}, {:?})",
                        name.connect(", "),
                        *inverted,
                        *osection,
                        *newlined)
            }
            _ => {
                format!("{:?}", token)
            }
        }
    }

    fn check_tokens(actual: Vec<Token>, expected: &[Token]) -> bool {
        // TODO: equality is currently broken for enums
        let actual: Vec<~str> = actual.iter().map(token_to_str).collect();
        let expected = expected.iter().map(token_to_str).collect();

        if actual != expected {
            error!("Found {:?}, but expected {:?}", actual.as_slice(), expected.as_slice());
            return false;
        }

        return true;
    }

    #[test]
    fn test_compile_texts() {
        assert!(check_tokens(compile_str("hello world").tokens, [
            Text(~"hello world")
        ]));
        assert!(check_tokens(compile_str("hello {world").tokens, [
            Text(~"hello {world")
        ]));
        assert!(check_tokens(compile_str("hello world}").tokens, [
            Text(~"hello world}")
        ]));
        assert!(check_tokens(compile_str("hello world}}").tokens, [
            Text(~"hello world}}")
        ]));
    }

    #[test]
    fn test_compile_etags() {
        assert!(check_tokens(compile_str("{{ name }}").tokens, [
            ETag(vec!(~"name"), ~"{{ name }}")
        ]));

        assert!(check_tokens(compile_str("before {{name}} after").tokens, [
            Text(~"before "),
            ETag(vec!(~"name"), ~"{{name}}"),
            Text(~" after")
        ]));

        assert!(check_tokens(compile_str("before {{name}}").tokens, [
            Text(~"before "),
            ETag(vec!(~"name"), ~"{{name}}")
        ]));

        assert!(check_tokens(compile_str("{{name}} after").tokens, [
            ETag(vec!(~"name"), ~"{{name}}"),
            Text(~" after")
        ]));
    }

    #[test]
    fn test_compile_utags() {
        assert!(check_tokens(compile_str("{{{name}}}").tokens, [
            UTag(vec!(~"name"), ~"{{{name}}}")
        ]));

        assert!(check_tokens(compile_str("before {{{name}}} after").tokens, [
            Text(~"before "),
            UTag(vec!(~"name"), ~"{{{name}}}"),
            Text(~" after")
        ]));

        assert!(check_tokens(compile_str("before {{{name}}}").tokens, [
            Text(~"before "),
            UTag(vec!(~"name"), ~"{{{name}}}")
        ]));

        assert!(check_tokens(compile_str("{{{name}}} after").tokens, [
            UTag(vec!(~"name"), ~"{{{name}}}"),
            Text(~" after")
        ]));
    }

    #[test]
    fn test_compile_sections() {
        assert!(check_tokens(compile_str("{{# name}}{{/name}}").tokens, [
            Section(
                vec!(~"name"),
                false,
                Vec::new(),
                ~"{{",
                ~"{{# name}}",
                ~"",
                ~"{{/name}}",
                ~"}}"
            )
        ]));

        assert!(check_tokens(compile_str("before {{^name}}{{/name}} after").tokens, [
            Text(~"before "),
            Section(
                vec!(~"name"),
                true,
                Vec::new(),
                ~"{{",
                ~"{{^name}}",
                ~"",
                ~"{{/name}}",
                ~"}}"
            ),
            Text(~" after")
        ]));

        assert!(check_tokens(compile_str("before {{#name}}{{/name}}").tokens, [
            Text(~"before "),
            Section(
                vec!(~"name"),
                false,
                Vec::new(),
                ~"{{",
                ~"{{#name}}",
                ~"",
                ~"{{/name}}",
                ~"}}"
            )
        ]));

        assert!(check_tokens(compile_str("{{#name}}{{/name}} after").tokens, [
            Section(
                vec!(~"name"),
                false,
                Vec::new(),
                ~"{{",
                ~"{{#name}}",
                ~"",
                ~"{{/name}}",
                ~"}}"
            ),
            Text(~" after")
        ]));

        assert!(check_tokens(compile_str(
                "before {{#a}} 1 {{^b}} 2 {{/b}} {{/a}} after").tokens, [
            Text(~"before "),
            Section(
                vec!(~"a"),
                false,
                vec!(
                    Text(~" 1 "),
                    Section(
                        vec!(~"b"),
                        true,
                        vec!(Text(~" 2 ")),
                        ~"{{",
                        ~"{{^b}}",
                        ~" 2 ",
                        ~"{{/b}}",
                        ~"}}"
                    ),
                    Text(~" ")
                ),
                ~"{{",
                ~"{{#a}}",
                ~" 1 {{^b}} 2 {{/b}} ",
                ~"{{/a}}",
                ~"}}"
            ),
            Text(~" after")
        ]));
    }

    #[test]
    fn test_compile_partials() {
        assert!(check_tokens(compile_str("{{> test}}").tokens, [
            Partial(~"test", ~"", ~"{{> test}}")
        ]));

        assert!(check_tokens(compile_str("before {{>test}} after").tokens, [
            Text(~"before "),
            Partial(~"test", ~"", ~"{{>test}}"),
            Text(~" after")
        ]));

        assert!(check_tokens(compile_str("before {{> test}}").tokens, [
            Text(~"before "),
            Partial(~"test", ~"", ~"{{> test}}")
        ]));

        assert!(check_tokens(compile_str("{{>test}} after").tokens, [
            Partial(~"test", ~"", ~"{{>test}}"),
            Text(~" after")
        ]));
    }

    #[test]
    fn test_compile_delimiters() {
        assert!(check_tokens(compile_str("before {{=<% %>=}}<%name%> after").tokens, [
            Text(~"before "),
            ETag(vec!(~"name"), ~"<%name%>"),
            Text(~" after")
        ]));
    }

    #[deriving(Encodable)]
    struct Name { name: ~str }

    #[test]
    fn test_render_texts() {
        let ctx = &Name { name: ~"world" };

        assert_eq!(render_str("hello world", ctx), ~"hello world");
        assert_eq!(render_str("hello {world", ctx), ~"hello {world");
        assert_eq!(render_str("hello world}", ctx), ~"hello world}");
        assert_eq!(render_str("hello {world}", ctx), ~"hello {world}");
        assert_eq!(render_str("hello world}}", ctx), ~"hello world}}");
    }

    #[test]
    fn test_render_etags() {
        let ctx = &Name { name: ~"world" };

        assert!(render_str("hello {{name}}", ctx) == ~"hello world");
    }

    #[test]
    fn test_render_utags() {
        let ctx = &Name { name: ~"world" };

        assert!(render_str("hello {{{name}}}", ctx) == ~"hello world");
    }

    #[test]
    fn test_render_sections() {
        let mut ctx0 = HashMap::new();
        let template = compile_str("0{{#a}}1 {{n}} 3{{/a}}5");

        assert!(template.render_data(Map(ctx0.clone())) == ~"05");

        ctx0.insert(~"a", Vec(Vec::new()));
        assert!(template.render_data(Map(ctx0.clone())) == ~"05");

        let ctx1: HashMap<~str, Data> = HashMap::new();
        ctx0.insert(~"a", Vec(vec!(Map(ctx1.clone()))));

        assert!(template.render_data(Map(ctx0.clone())) == ~"01  35");

        let mut ctx1 = HashMap::new();
        ctx1.insert(~"n", Str(~"a"));
        ctx0.insert(~"a", Vec(vec!(Map(ctx1.clone()))));
        assert!(template.render_data(Map(ctx0.clone())) == ~"01 a 35");

        //ctx0.insert(~"a", Fun(|_text| {~"foo"}));
        //assert!(template.render_data(Map(ctx0)) == ~"0foo5");
    }

    #[test]
    fn test_render_inverted_sections() {
        let template = compile_str("0{{^a}}1 3{{/a}}5");

        let mut ctx0 = HashMap::new();
        assert!(template.render_data(Map(ctx0.clone())) == ~"01 35");

        ctx0.insert(~"a", Vec(vec!()));
        assert!(template.render_data(Map(ctx0.clone())) == ~"01 35");

        let mut ctx1 = HashMap::new();
        ctx0.insert(~"a", Vec(vec!(Map(ctx1.clone()))));
        assert!(template.render_data(Map(ctx0.clone())) == ~"05");

        ctx1.insert(~"n", Str(~"a"));
        assert!(template.render_data(Map(ctx0.clone())) == ~"05");
    }

    #[test]
    fn test_render_partial() {
        let template = Context::new(Path::new("src/test-data"))
            .compile_path(Path::new("base"))
            .unwrap();

        let mut ctx0 = HashMap::new();
        assert_eq!(template.render_data(Map(ctx0.clone())), ~"<h2>Names</h2>\n");

        ctx0.insert(~"names", Vec(vec!()));
        assert_eq!(template.render_data(Map(ctx0.clone())), ~"<h2>Names</h2>\n");

        let mut ctx1 = HashMap::new();
        ctx0.insert(~"names", Vec(vec!(Map(ctx1.clone()))));
        assert_eq!(
            template.render_data(Map(ctx0.clone())),
            ~"<h2>Names</h2>\n  <strong></strong>\n\n");

        ctx1.insert(~"name", Str(~"a"));
        ctx0.insert(~"names", Vec(vec!(Map(ctx1.clone()))));
        assert_eq!(
            template.render_data(Map(ctx0.clone())),
            ~"<h2>Names</h2>\n  <strong>a</strong>\n\n");

        let mut ctx2 = HashMap::new();
        ctx2.insert(~"name", Str(~"<b>"));
        ctx0.insert(~"names", Vec(vec!(Map(ctx1), Map(ctx2))));
        assert_eq!(
            template.render_data(Map(ctx0)),
            ~"<h2>Names</h2>\n  <strong>a</strong>\n\n  <strong>&lt;b&gt;</strong>\n\n");
    }

    fn parse_spec_tests(src: &str) -> Vec<json::Json> {
        let path = Path::new(src);

        let file_contents = match File::open(&path).read_to_end() {
            Ok(reader) => reader,
            Err(e) => fail!("Could not read file {}", e),
        };

        let s = match str::from_utf8_owned(file_contents){
            Some(str) => str,
            None => {fail!("File was not UTF8 encoded");}
        };

        match json::from_str(s) {
            Err(e) => fail!(e.to_str()),
            Ok(json) => {
                match json {
                    json::Object(d) => {
                        let mut d = d;
                        match d.pop(&~"tests") {
                            Some(json::List(tests)) => tests.move_iter().collect(),
                            _ => fail!("{}: tests key not a list", src),
                        }
                    }
                    _ => fail!("{}: JSON value not a map", src),
                }
            }
        }
    }

//    fn convert_json_map(map: json::Object) -> HashMap<~str, Data> {
//        let mut d = HashMap::new();
//        for (key, value) in map.move_iter() {
//            d.insert(key.to_owned(), convert_json(value));
//        }
//        d
//    }
//
//    fn convert_json(value: json::Json) -> Data {
//        match value {
//          json::Number(n) => {
//            // We have to cheat and use {:?} because %f doesn't convert 3.3 to
//            // 3.3.
//            Str(fmt!("{:?}", n))
//          }
//          json::String(s) => { Str(s.to_owned()) }
//          json::Boolean(b) => { Bool(b) }
//          json::List(v) => { Vec(v.map(convert_json)) }
//          json::Object(d) => { Map(convert_json_map(d)) }
//          _ => { fail!("{:?}", value) }
//        }
//    }

    fn write_partials(tmpdir: &Path, value: &json::Json) {
        match value {
            &json::Object(ref d) => {
                for (key, value) in d.iter() {
                    match value {
                        &json::String(ref s) => {
                            let mut path = tmpdir.clone();
                            path.push(*key + ".mustache");
                            File::create(&path).write(s.as_bytes());
                            // match File::create(&path).write(s.as_bytes()) {
                            //     Some(mut wr) => wr.write(s.as_bytes()),
                            //     None => fail!(),
                            // }
                        }
                        _ => fail!(),
                    }
                }
            },
            _ => fail!(),
        }
    }

    fn run_test(test: ~json::Object, data: Data) {
        let template = match test.find(&~"template") {
            Some(&json::String(ref s)) => s.clone(),
            _ => fail!(),
        };

        let expected = match test.find(&~"expected") {
            Some(&json::String(ref s)) => s.clone(),
            _ => fail!(),
        };

        // Make a temporary dir where we'll store our partials. This is to
        // avoid a race on filenames.
        let tmpdir = match TempDir::new("") {
            Some(tmpdir) => tmpdir,
            None => fail!(),
        };

        match test.find(&~"partials") {
            Some(value) => write_partials(tmpdir.path(), value),
            None => {},
        }

        let ctx = Context::new(tmpdir.path().clone());
        let template = ctx.compile(template.chars());
        let result = template.render_data(data);

        if result != expected {
            println!("desc:     {}", test.find(&~"desc").unwrap().to_str());
            println!("context:  {}", test.find(&~"data").unwrap().to_str());
            println!("=>");
            println!("template: {:?}", template);
            println!("expected: {:?}", expected);
            println!("actual:   {:?}", result);
            println!("");
        }
        assert_eq!(result, expected);
    }

    fn run_tests(spec: &str) {
        for json in parse_spec_tests(spec).move_iter() {
            let test = match json {
                json::Object(m) => m,
                _ => fail!(),
            };

            let data = match test.find(&~"data") {
                Some(data) => data.clone(),
                None => fail!(),
            };

            let mut encoder = Encoder::new();
            data.encode(&mut encoder);
            assert_eq!(encoder.data.len(), 1);

            run_test(test, encoder.data.pop().unwrap());
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

//    #[test]
//    fn test_spec_lambdas() {
//        for json in parse_spec_tests(~"spec/specs/~lambdas.json").iter() {
//            let test = match json {
//                &json::Object(m) => m,
//                _ => fail!(),
//            };
//
//            // Replace the lambda with rust code.
//            let data = match test.find(&~"data") {
//                Some(data) => (*data).clone(),
//                None => fail!(),
//            };
//
//            let encoder = Encoder::new();
//            data.encode(&encoder);
//            let ctx = match encoder.data {
//                [Map(ctx)] => ctx,
//                _ => fail!(),
//            };
//
//            let f = match *test.find(&~"name").unwrap() {
//                json::String(~"Interpolation") => {
//                    |_text| {~"world" }
//                }
//                json::String(~"Interpolation - Expansion") => {
//                    |_text| {~"{{planet}}" }
//                }
//                json::String(~"Interpolation - Alternate Delimiters") => {
//                    |_text| {~"|planet| => {{planet}}" }
//                }
//                json::String(~"Interpolation - Multiple Calls") => {
//                    let calls = 0i;
//                    |_text| {*calls = *calls + 1; calls.to_str() }
//                }
//                json::String(~"Escaping") => {
//                    |_text| {~">" }
//                }
//                json::String(~"Section") => {
//                    |text: ~str| {if *text == ~"{{x}}" { ~"yes" } else { ~"no" } }
//                }
//                json::String(~"Section - Expansion") => {
//                    |text: ~str| {*text + "{{planet}}" + *text }
//                }
//                json::String(~"Section - Alternate Delimiters") => {
//                    |text: ~str| {*text + "{{planet}} => |planet|" + *text }
//                }
//                json::String(~"Section - Multiple Calls") => {
//                    |text: ~str| {~"__" + *text + ~"__" }
//                }
//                json::String(~"Inverted Section") => {
//                    |_text| {~"" }
//                }
//                value => { fail!("{:?}", value) }
//            };
//            //ctx.insert(~"lambda", Fun(f));
//
//            run_test(test, Map(ctx));
//        }
//    }
}
