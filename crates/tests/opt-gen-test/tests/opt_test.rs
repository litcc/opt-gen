#[cfg(test)]
mod test {
    use std::fmt;

    use opt_gen::{CowOpt, RawOpt};
    use serde::de;
    use serde::de::Visitor;
    use serde::Deserialize;
    use serde::Serialize;
    use serde_json::json;

    /// 一个 u64 的拓展解析器
    pub struct U64Visitor;

    impl U64Visitor {
        #[allow(dead_code)]
        pub fn try_from_str(s: &str) -> Option<u64> {
            s.parse().ok()
        }
    }

    impl<'de> Visitor<'de> for U64Visitor {
        type Value = u64;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("err")
        }

        fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(value)
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            value.parse().map_err(de::Error::custom)
        }
    }

    /// 一个 Option<u64> 的拓展解析器
    pub struct OptU64Visitor;

    impl<'de> Visitor<'de> for OptU64Visitor {
        type Value = Option<u64>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("err")
        }

        fn visit_none<E>(self) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(None)
        }

        fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
        where
            D: de::Deserializer<'de>,
        {
            deserializer.deserialize_any(U64Visitor).map(Some)
        }
    }

    pub fn deserialize_optional_u64<'de, D>(
        deserializer: D,
    ) -> Result<Option<u64>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_option(OptU64Visitor)
    }

    pub fn deserialize_str_u64<'de, D>(deserializer: D) -> Result<u64, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(U64Visitor)
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct A {
        pub abc: String,
        pub def: u64,
    }

    #[derive(RawOpt)]
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Test {
        #[serde(rename = "id")]
        pub name: String,
        #[serde(deserialize_with = "deserialize_str_u64")]
        pub age: u64,
        pub sex: bool,

        #[serde(default, deserialize_with = "deserialize_optional_u64")]
        #[opt_gen(Cow)]
        pub data: Option<u64>,

        pub data234: A,
    }


    #[derive(CowOpt)]
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Test2 {
        #[serde(rename = "id")]
        pub name: String,
        #[serde(deserialize_with = "deserialize_str_u64")]
        pub age: u64,
        pub sex: bool,

        #[serde(default, deserialize_with = "deserialize_optional_u64")]
        #[opt_gen(Raw)]
        pub data: Option<u64>,

        pub data234: A,
    }

    #[test]
    pub fn asdfasdf() {
        let json = json!({
            "id":"1231",
            "age": 2,
            "sex":false,
            "data": "1123123123"
        });

        let fdf = serde_json::from_value::<Test>(json);
        println!("data1: {:#?}", fdf);

        let json = json!({
            "id":"1231",
            "age": "1231232",
            "sex":false,
            "data": null
        });

        let fdf = serde_json::from_value::<TestRef>(json);
        println!("data2: {:#?}", fdf);
    }
}
