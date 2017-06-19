use std::collections::HashMap;

use mustache::{StrVal, Bool, VecVal, Map, Fun};
use mustache::{MapBuilder, VecBuilder};

#[test]
fn test_empty_builders() {
    assert_eq!(MapBuilder::new().build(), Map(HashMap::new()));

    assert_eq!(VecBuilder::new().build(), VecVal(Vec::new()));
}

#[test]
fn test_builders() {
    let mut pride_and_prejudice = HashMap::new();
    pride_and_prejudice.insert("title".to_string(),
                               StrVal("Pride and Prejudice".to_string()));
    pride_and_prejudice.insert("publish_date".to_string(), StrVal("1813".to_string()));

    let mut m = HashMap::new();
    m.insert("first_name".to_string(), StrVal("Jane".to_string()));
    m.insert("last_name".to_string(), StrVal("Austen".to_string()));
    m.insert("age".to_string(), StrVal("41".to_string()));
    m.insert("died".to_string(), Bool(true));
    m.insert("works".to_string(),
             VecVal(vec![StrVal("Sense and Sensibility".to_string()),
                         Map(pride_and_prejudice)]));

    assert_eq!(MapBuilder::new()
                   .insert_str("first_name", "Jane")
                   .insert_str("last_name", "Austen")
                   .insert("age", &41usize).expect("age")
                   .insert_bool("died", true)
                   .insert_vec("works", |builder| {
            builder.push_str("Sense and Sensibility").push_map(|builder| {
                builder.insert_str("title", "Pride and Prejudice")
                    .insert("publish_date", &1813usize).expect("publish_date")
            })
        })
                   .build(),
               Map(m));
}

#[test]
fn test_map_fn_builder() {
    // We can't directly compare closures, so just make sure we thread
    // through the builder.

    let mut count = 0usize;
    let data = MapBuilder::new()
        .insert_fn("count".to_string(), move |s| {
            count += 1usize;
            s.clone() + &count.to_string()
        })
        .build();

    assert_let!(Map(m) = data => {
        assert_let!(Some(&Fun(ref f)) = m.get("count") => {
            let f = &mut *f.borrow_mut();
            assert_eq!((*f)("count: ".to_string()), "count: 1".to_string());
            assert_eq!((*f)("count: ".to_string()), "count: 2".to_string());
            assert_eq!((*f)("count: ".to_string()), "count: 3".to_string());
        });
    })
}

#[test]
fn test_vec_fn_builder() {
    // We can't directly compare closures, so just make sure we thread
    // through the builder.

    let mut count = 0usize;
    let data = VecBuilder::new()
        .push_fn(move |s| {
            count += 1usize;
            s + &count.to_string()
        })
        .build();

    assert_let!(VecVal(vs) = data => {
        let mut iter = vs.iter();

        assert_let!(Some(&Fun(ref f)) = iter.next() => {
            let f = &mut *f.borrow_mut();
            assert_eq!((*f)("count: ".to_string()), "count: 1".to_string());
            assert_eq!((*f)("count: ".to_string()), "count: 2".to_string());
            assert_eq!((*f)("count: ".to_string()), "count: 3".to_string());
        });

        assert_eq!(iter.next(), None);
    })
}
