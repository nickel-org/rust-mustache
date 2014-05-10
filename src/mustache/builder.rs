use std::cell::RefCell;
use collections::HashMap;
use serialize::Encodable;

use encoder;
use encoder::{Encoder, Error};
use super::{Data, Str, Bool, Vec, Map, Fun};

/// `MapBuilder` is a helper type that construct `Data` types.
pub struct MapBuilder<'a> {
    data: HashMap<~str, Data<'a>>,
}

impl<'a> MapBuilder<'a> {
    /// Create a `MapBuilder`
    #[inline]
    pub fn new() -> MapBuilder<'a> {
        MapBuilder {
            data: HashMap::new(),
        }
    }

    /// Add an `Encodable` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert("name", &("Jane Austen")).unwrap()
    ///     .insert("age", &41).unwrap()
    ///     .build();
    /// ```
    #[inline]
    pub fn insert<
        K: StrAllocating, T: Encodable<Encoder<'a>, Error>
    >(self, key: K, value: &T) -> Result<MapBuilder<'a>, Error> {
        let MapBuilder { mut data } = self;
        let value = try!(encoder::encode(value));
        data.insert(key.into_owned(), value);
        Ok(MapBuilder { data: data })
    }

    /// Add a `~str` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert_str("name", "Jane Austen")
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_str<
        K: StrAllocating, V: StrAllocating
    >(self, key: K, value: V) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        data.insert(key.into_owned(), Str(value.into_owned()));
        MapBuilder { data: data }
    }

    /// Add a `bool` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert_bool("show", true)
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_bool<K: StrAllocating>(self, key: K, value: bool) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        data.insert(key.into_owned(), Bool(value));
        MapBuilder { data: data }
    }

    /// Add a `Vec` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert_vec("authors", |builder| {
    ///         builder
    ///             .push_str("Jane Austen")
    ///             .push_str("Lewis Carroll")
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_vec<K: StrAllocating>(self, key: K, f: |VecBuilder<'a>| -> VecBuilder<'a>) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        let builder = f(VecBuilder::new());
        data.insert(key.into_owned(), builder.build());
        MapBuilder { data: data }
    }

    /// Add a `Map` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert_map("person1", |builder| {
    ///         builder
    ///             .insert_str("first_name", "Jane")
    ///             .insert_str("last_name", "Austen")
    ///     })
    ///     .insert_map("person2", |builder| {
    ///         builder
    ///             .insert_str("first_name", "Lewis")
    ///             .insert_str("last_name", "Carroll")
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_map<K: StrAllocating>(self, key: K, f: |MapBuilder<'a>| -> MapBuilder<'a>) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        let builder = f(MapBuilder::new());
        data.insert(key.into_owned(), builder.build());
        MapBuilder { data: data }
    }

    /// Add a function to the `MapBuilder`.
    ///
    /// ```rust
    /// let mut count = 0;
    /// let data = MapBuilder::new()
    ///     .insert_fn("increment", |_| {
    ///         count += 1;
    ///         count.to_str()
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_fn<K: StrAllocating>(self, key: K, f: |~str|: 'a -> ~str) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        data.insert(key.into_owned(), Fun(RefCell::new(f)));
        MapBuilder { data: data }
    }

    /// Return the built `Data`.
    #[inline]
    pub fn build(self) -> Data<'a> {
        Map(self.data)
    }
}

pub struct VecBuilder<'a> {
    data: Vec<Data<'a>>,
}

impl<'a> VecBuilder<'a> {
    /// Create a `VecBuilder`
    #[inline]
    pub fn new() -> VecBuilder<'a> {
        VecBuilder {
            data: Vec::new(),
        }
    }

    /// Add an `Encodable` to the `VecBuilder`.
    ///
    /// ```rust
    /// let data = VecBuilder::new()
    ///     .push(& &"Jane Austen").unwrap()
    ///     .push(&41).unwrap()
    ///     .build();
    /// ```
    #[inline]
    pub fn push<
        T: Encodable<Encoder<'a>, Error>
    >(self, value: &T) -> Result<VecBuilder<'a>, Error> {
        let VecBuilder { mut data } = self;
        let value = try!(encoder::encode(value));
        data.push(value);
        Ok(VecBuilder { data: data })
    }

    /// Add a `~str` to the `VecBuilder`.
    ///
    /// ```rust
    /// let data = VecBuilder::new()
    ///     .push_str("Jane Austen")
    ///     .push_str("Lewis Carroll")
    ///     .build();
    /// ```
    #[inline]
    pub fn push_str<T: StrAllocating>(self, value: T) -> VecBuilder<'a> {
        let VecBuilder { mut data } = self;
        data.push(Str(value.into_owned()));
        VecBuilder { data: data }
    }

    /// Add a `bool` to the `VecBuilder`.
    ///
    /// ```rust
    /// let data = VecBuilder::new()
    ///     .insert_bool(false)
    ///     .insert_bool(true)
    ///     .build();
    /// ```
    #[inline]
    pub fn push_bool(self, value: bool) -> VecBuilder<'a> {
        let VecBuilder { mut data } = self;
        data.push(Bool(value));
        VecBuilder { data: data }
    }

    /// Add a `Vec` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = VecBuilder::new()
    ///     .insert_vec(|builder| {
    ///         builder
    ///             .push_str(~"Jane Austen")
    ///             .push_str(~"Lewis Carroll")
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn push_vec(self, f: |VecBuilder<'a>| -> VecBuilder<'a>) -> VecBuilder<'a> {
        let VecBuilder { mut data } = self;
        let builder = f(VecBuilder::new());
        data.push(builder.build());
        VecBuilder { data: data }
    }

    /// Add a `Map` to the `VecBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert_map(|builder| {
    ///         builder
    ///             .insert_str(~"first_name", ~"Jane")
    ///             .insert_str(~"last_name", ~"Austen")
    ///     })
    ///     .insert_map(|builder| {
    ///         builder
    ///             .insert_str(~"first_name", ~"Lewis")
    ///             .insert_str(~"last_name", ~"Carroll")
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn push_map(self, f: |MapBuilder<'a>| -> MapBuilder<'a>) -> VecBuilder<'a> {
        let VecBuilder { mut data } = self;
        let builder = f(MapBuilder::new());
        data.push(builder.build());
        VecBuilder { data: data }
    }

    /// Add a function to the `VecBuilder`.
    ///
    /// ```rust
    /// let mut count = 0;
    /// let data = VecBuilder::new()
    ///     .push_fn(|s| {
    ///         count += 1;
    ///         s + count.to_str()
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn push_fn(self, f: |~str|: 'a -> ~str) -> VecBuilder<'a> {
        let VecBuilder { mut data } = self;
        data.push(Fun(RefCell::new(f)));
        VecBuilder { data: data }
    }

    #[inline]
    pub fn build(self) -> Data<'a> {
        Vec(self.data)
    }
}

#[cfg(test)]
mod tests {
    use collections::HashMap;

    use super::super::{Str, Bool, Vec, Map, Fun};
    use super::{MapBuilder, VecBuilder};

    #[test]
    fn test_empty_builders() {
        assert_eq!(
            MapBuilder::new().build(),
            Map(HashMap::new()));

        assert_eq!(
            VecBuilder::new().build(),
            Vec(Vec::new()));
    }

    #[test]
    fn test_builders() {
        let mut pride_and_prejudice = HashMap::new();
        pride_and_prejudice.insert("title".to_owned(), Str("Pride and Prejudice".to_owned()));
        pride_and_prejudice.insert("publish_date".to_owned(), Str("1813".to_owned()));

        let mut m = HashMap::new();
        m.insert("first_name".to_owned(), Str("Jane".to_owned()));
        m.insert("last_name".to_owned(), Str("Austen".to_owned()));
        m.insert("age".to_owned(), Str("41".to_owned()));
        m.insert("died".to_owned(), Bool(true));
        m.insert("works".to_owned(), Vec(vec!(
            Str("Sense and Sensibility".to_owned()),
            Map(pride_and_prejudice))));

        assert_eq!(
            MapBuilder::new()
                .insert_str("first_name", "Jane")
                .insert_str("last_name", "Austen")
                .insert("age", &41).unwrap()
                .insert_bool("died", true)
                .insert_vec("works", |builder| {
                    builder
                        .push_str("Sense and Sensibility")
                        .push_map(|builder| {
                            builder
                                .insert_str("title", "Pride and Prejudice")
                                .insert("publish_date", &1813).unwrap()
                        })
                })
                .build(),
            Map(m));
    }

    #[test]
    fn test_map_fn_builder() {
        // We can't directly compare closures, so just make sure we thread
        // through the builder.

        let mut count = 0;
        let data = MapBuilder::new()
            .insert_fn("count", |s| {
                count += 1;
                s + count.to_str()
            })
            .build();

        match data {
            Map(m) => {
                match *m.find_equiv(&("count")).unwrap() {
                    Fun(ref f) => {
                        let f = &mut *f.borrow_mut();
                        assert_eq!((*f)("count: ".to_owned()), "count: 1".to_owned());
                        assert_eq!((*f)("count: ".to_owned()), "count: 2".to_owned());
                        assert_eq!((*f)("count: ".to_owned()), "count: 3".to_owned());
                    }
                    _ => fail!(),
                }
            }
            _ => fail!(),
        }
    }

    #[test]
    fn test_vec_fn_builder() {
        // We can't directly compare closures, so just make sure we thread
        // through the builder.

        let mut count = 0;
        let data = VecBuilder::new()
            .push_fn(|s| {
                count += 1;
                s + count.to_str()
            })
            .build();

        match data {
            Vec(vs) => {
                match vs.as_slice() {
                    [Fun(ref f)] => {
                        let f = &mut *f.borrow_mut();
                        assert_eq!((*f)("count: ".to_owned()), "count: 1".to_owned());
                        assert_eq!((*f)("count: ".to_owned()), "count: 2".to_owned());
                        assert_eq!((*f)("count: ".to_owned()), "count: 3".to_owned());
                    }
                    _ => fail!(),
                }
            }
            _ => fail!(),
        }
    }
}
