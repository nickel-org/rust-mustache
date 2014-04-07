use collections::HashMap;
use serialize::Encodable;

use encoder;
use encoder::{Encoder, Error};
use super::{Data, Str, Bool, Vec, Map, Fun};

///
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
    ///     .insert(~"name", & &"Jane Austen").unwrap()
    ///     .insert(~"age", &41).unwrap()
    ///     .build();
    /// ```
    #[inline]
    pub fn insert<
        T: Encodable<Encoder<'a>, Error>
    >(self, key: ~str, value: &T) -> Result<MapBuilder<'a>, Error> {
        let MapBuilder { mut data } = self;
        let value = try!(encoder::encode(value));
        data.insert(key, value);
        Ok(MapBuilder { data: data })
    }

    /// Add a `~str` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert_str(~"name", ~"Jane Austen")
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_str(self, key: ~str, value: ~str) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        data.insert(key, Str(value));
        MapBuilder { data: data }
    }

    /// Add a `bool` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert_bool(~"show", true)
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_bool(self, key: ~str, value: bool) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        data.insert(key, Bool(value));
        MapBuilder { data: data }
    }

    /// Add a `Vec` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert_vec(~"authors", |builder| {
    ///         builder
    ///             .push_str(~"Jane Austen")
    ///             .push_str(~"Lewis Carroll")
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_vec(self, key: ~str, f: |VecBuilder<'a>| -> VecBuilder<'a>) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        let builder = f(VecBuilder::new());
        data.insert(key, builder.build());
        MapBuilder { data: data }
    }

    /// Add a `Map` to the `MapBuilder`.
    ///
    /// ```rust
    /// let data = MapBuilder::new()
    ///     .insert_map(~"person1", |builder| {
    ///         builder
    ///             .insert_str(~"first_name", ~"Jane")
    ///             .insert_str(~"last_name", ~"Austen")
    ///     })
    ///     .insert_map(~"person2", |builder| {
    ///         builder
    ///             .insert_str(~"first_name", ~"Lewis")
    ///             .insert_str(~"last_name", ~"Carroll")
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_map(self, key: ~str, f: |MapBuilder<'a>| -> MapBuilder<'a>) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        let builder = f(MapBuilder::new());
        data.insert(key, builder.build());
        MapBuilder { data: data }
    }

    /// Add a function to the `MapBuilder`.
    ///
    /// ```rust
    /// let mut count = 0;
    /// let data = MapBuilder::new()
    ///     .insert_fn(~"increment", |_| {
    ///         count += 1;
    ///         count.to_str()
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_fn(self, key: ~str, f: 'a |~str| -> ~str) -> MapBuilder<'a> {
        let MapBuilder { mut data } = self;
        data.insert(key, Fun(f));
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
    ///     .push_str(~"Jane Austen")
    ///     .push_str(~"Lewis Carroll")
    ///     .build();
    /// ```
    #[inline]
    pub fn push_str(self, value: ~str) -> VecBuilder<'a> {
        let VecBuilder { mut data } = self;
        data.push(Str(value));
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
    pub fn push_fn(self, f: 'a |~str| -> ~str) -> VecBuilder<'a> {
        let VecBuilder { mut data } = self;
        data.push(Fun(f));
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
    fn test_builders() {
        assert_eq!(
            MapBuilder::new()
                .build(),
            Map(HashMap::new()));

        let mut pride_and_prejudice = HashMap::new();
        pride_and_prejudice.insert(~"title", Str(~"Pride and Prejudice"));
        pride_and_prejudice.insert(~"publish_date", Str(~"1813"));

        let mut m = HashMap::new();
        m.insert(~"first_name", Str(~"Jane"));
        m.insert(~"last_name", Str(~"Austen"));
        m.insert(~"age", Str(~"41"));
        m.insert(~"died", Bool(true));
        m.insert(~"works", Vec(vec!(
            Str(~"Sense and Sensibility"),
            Map(pride_and_prejudice))));

        assert_eq!(
            MapBuilder::new()
                .insert_str(~"first_name", ~"Jane")
                .insert_str(~"last_name", ~"Austen")
                .insert(~"age", &41).unwrap()
                .insert_bool(~"died", true)
                .insert_vec(~"works", |builder| {
                    builder
                        .push_str(~"Sense and Sensibility")
                        .push_map(|builder| {
                            builder
                                .insert_str(~"title", ~"Pride and Prejudice")
                                .insert(~"publish_date", &1813).unwrap()
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
            .insert_fn(~"count", |s| {
                count += 1;
                s + count.to_str()
            })
            .build();

        match data {
            Map(m) => {
                match *m.find_equiv(& &"count").unwrap() {
                    Fun(ref f) => {
                        assert_eq!((*f)(~"count: "), ~"count: 1");
                        assert_eq!((*f)(~"count: "), ~"count: 2");
                        assert_eq!((*f)(~"count: "), ~"count: 3");
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
                        assert_eq!((*f)(~"count: "), ~"count: 1");
                        assert_eq!((*f)(~"count: "), ~"count: 2");
                        assert_eq!((*f)(~"count: "), ~"count: 3");
                    }
                    _ => fail!(),
                }
            }
            _ => fail!(),
        }
    }
}
