use crate::PathScheme;

struct PathSchemeVisitor;

impl<'de> serde::Deserialize<'de> for PathScheme {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(PathSchemeVisitor)
    }
}

impl serde::Serialize for PathScheme {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> serde::de::Visitor<'de> for PathSchemeVisitor {
    type Value = PathScheme;

    fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("a path scheme")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        v.parse().map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
#[test]
fn roundtrip() {
    use serde_test::{assert_tokens, Token};

    for p in &["/", "/users/:id", "/users/:id/face", "/users/:id/**"] {
        assert_tokens(&p.parse::<PathScheme>().unwrap(), &[Token::Str(p)]);
    }
}
