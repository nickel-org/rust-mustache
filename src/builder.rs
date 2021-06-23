use std::cell::RefCell;
use std::string::ToString;
use std::collections::HashMap;
use serde::Serialize;

use encoder::Error;
use super::{Data, to_data};

/// `MapBuilder` is a helper type that construct `Data` types.
#[derive(Default)]
pub struct MapBuilder {
    data: HashMap<String, Data>,
}

impl MapBuilder {
    /// Create a `MapBuilder`
    #[inline]
    pub fn new() -> MapBuilder {
        MapBuilder::default()
    }

    /// Add an `Encodable` to the `MapBuilder`.
    ///
    /// ```rust
    /// use mustache::MapBuilder;
    /// let data = MapBuilder::new()
    ///     .insert("name", &("Jane Austen")).expect("Failed to encode name")
    ///     .insert("age", &41usize).expect("Failed to encode age")
    ///     .build();
    /// ```
    #[inline]
    pub fn insert<K, T>(self, key: K, value: &T) -> Result<MapBuilder, Error>
    where
        K: Into<String>,
        T: Serialize,
    {
        let MapBuilder { mut data } = self;
        let value = to_data(value)?;
        data.insert(key.into(), value);
        Ok(MapBuilder { data })
    }

    /// Add a `String` to the `MapBuilder`.
    ///
    /// ```rust
    /// use mustache::MapBuilder;
    /// let data = MapBuilder::new()
    ///     .insert_str("name", "Jane Austen")
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_str<K, V>(self, key: K, value: V) -> MapBuilder
    where
        K: Into<String>,
        V: Into<String>,
    {
        let MapBuilder { mut data } = self;
        data.insert(key.into(), Data::String(value.into()));
        MapBuilder { data }
    }

    /// Add a `bool` to the `MapBuilder`.
    ///
    /// ```rust
    /// use mustache::MapBuilder;
    /// let data = MapBuilder::new()
    ///     .insert_bool("show", true)
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_bool<K>(self, key: K, value: bool) -> MapBuilder
    where
        K: Into<String>,
    {
        let MapBuilder { mut data } = self;
        data.insert(key.into(), Data::Bool(value));
        MapBuilder { data }
    }

    /// Add a `Vec` to the `MapBuilder`.
    ///
    /// ```rust
    /// use mustache::MapBuilder;
    /// let data = MapBuilder::new()
    ///     .insert_vec("authors", |builder| {
    ///         builder
    ///             .push_str("Jane Austen")
    ///             .push_str("Lewis Carroll")
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_vec<K, F>(self, key: K, mut f: F) -> MapBuilder
    where K: Into<String>,
          F: FnMut(VecBuilder) -> VecBuilder
    {
        let MapBuilder { mut data } = self;
        let builder = f(VecBuilder::new());
        data.insert(key.into(), builder.build());
        MapBuilder { data }
    }

    /// Add a `Map` to the `MapBuilder`.
    ///
    /// ```rust
    /// use mustache::MapBuilder;
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
    pub fn insert_map<K, F>(self, key: K, mut f: F) -> MapBuilder
    where
        K: Into<String>,
        F: FnMut(MapBuilder) -> MapBuilder,
    {
        let MapBuilder { mut data } = self;
        let builder = f(MapBuilder::new());
        data.insert(key.into(), builder.build());
        MapBuilder { data }
    }

    /// Add a function to the `MapBuilder`.
    ///
    /// ```rust
    /// use mustache::MapBuilder;
    /// let mut count = 0;
    /// let data = MapBuilder::new()
    ///     .insert_fn("increment", move |_| {
    ///         count += 1usize;
    ///         count.to_string()
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn insert_fn<K: ToString, F>(self, key: K, f: F) -> MapBuilder
    where F: FnMut(String) -> String + Send + 'static
    {
        let MapBuilder { mut data } = self;
        data.insert(key.to_string(), Data::Fun(RefCell::new(Box::new(f))));
        MapBuilder { data }
    }

    /// Return the built `Data`.
    #[inline]
    pub fn build(self) -> Data {
        Data::Map(self.data)
    }
}

#[derive(Default)]
pub struct VecBuilder {
    data: Vec<Data>,
}

impl VecBuilder {
    /// Create a `VecBuilder`
    #[inline]
    pub fn new() -> VecBuilder {
        VecBuilder::default()
    }

    /// Add an `Encodable` to the `VecBuilder`.
    ///
    /// ```rust
    /// use mustache::{VecBuilder, Data};
    /// let data: Data = VecBuilder::new()
    ///     .push(& &"Jane Austen").unwrap()
    ///     .push(&41usize).unwrap()
    ///     .build();
    /// ```
    #[inline]
    pub fn push<T: Serialize>(self, value: &T) -> Result<VecBuilder, Error> {
        let VecBuilder { mut data } = self;
        let value = to_data(value)?;
        data.push(value);
        Ok(VecBuilder { data })
    }

    /// Add a `String` to the `VecBuilder`.
    ///
    /// ```rust
    /// use mustache::VecBuilder;
    /// let data = VecBuilder::new()
    ///     .push_str("Jane Austen")
    ///     .push_str("Lewis Carroll")
    ///     .build();
    /// ```
    #[inline]
    pub fn push_str<T: ToString>(self, value: T) -> VecBuilder {
        let VecBuilder { mut data } = self;
        data.push(Data::String(value.to_string()));
        VecBuilder { data }
    }

    /// Add a `bool` to the `VecBuilder`.
    ///
    /// ```rust
    /// use mustache::VecBuilder;
    /// let data = VecBuilder::new()
    ///     .push_bool(false)
    ///     .push_bool(true)
    ///     .build();
    /// ```
    #[inline]
    pub fn push_bool(self, value: bool) -> VecBuilder {
        let VecBuilder { mut data } = self;
        data.push(Data::Bool(value));
        VecBuilder { data }
    }

    /// Add a `Vec` to the `MapBuilder`.
    ///
    /// ```rust
    /// use mustache::VecBuilder;
    /// let data = VecBuilder::new()
    ///     .push_vec(|builder| {
    ///         builder
    ///             .push_str("Jane Austen".to_string())
    ///             .push_str("Lewis Carroll".to_string())
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn push_vec<F>(self, mut f: F) -> VecBuilder
    where F: FnMut(VecBuilder) -> VecBuilder
    {
        let VecBuilder { mut data } = self;
        let builder = f(VecBuilder::new());
        data.push(builder.build());
        VecBuilder { data }
    }

    /// Add a `Map` to the `VecBuilder`.
    ///
    /// ```rust
    /// use mustache::VecBuilder;
    /// let data = VecBuilder::new()
    ///     .push_map(|builder| {
    ///         builder
    ///             .insert_str("first_name".to_string(), "Jane".to_string())
    ///             .insert_str("last_name".to_string(), "Austen".to_string())
    ///     })
    ///     .push_map(|builder| {
    ///         builder
    ///             .insert_str("first_name".to_string(), "Lewis".to_string())
    ///             .insert_str("last_name".to_string(), "Carroll".to_string())
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn push_map<F>(self, mut f: F) -> VecBuilder
    where F: FnMut(MapBuilder) -> MapBuilder
    {
        let VecBuilder { mut data } = self;
        let builder = f(MapBuilder::new());
        data.push(builder.build());
        VecBuilder { data }
    }

    /// Add a function to the `VecBuilder`.
    ///
    /// ```rust
    /// use mustache::VecBuilder;
    /// let mut count = 0;
    /// let data = VecBuilder::new()
    ///     .push_fn(move |s| {
    ///         count += 1usize;
    ///         s + &count.to_string()
    ///     })
    ///     .build();
    /// ```
    #[inline]
    pub fn push_fn<F>(self, f: F) -> VecBuilder
    where F: FnMut(String) -> String + Send + 'static
    {
        let VecBuilder { mut data } = self;
        data.push(Data::Fun(RefCell::new(Box::new(f))));
        VecBuilder { data }
    }

    #[inline]
    pub fn build(self) -> Data {
        Data::Vec(self.data)
    }
}
