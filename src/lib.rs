//! A basic path-matching language.
//!
//! ## Examples
//!
//! Path schemes may match literal paths:
//!
//! ```
//! # use pathscheme::*;
//! assert!("/users/new".parse::<PathScheme>()
//!     .unwrap()
//!     .matches("/users/new")
//!     .unwrap()
//!     .is_empty());
//! ```
//!
//! Or they may capture path segments:
//!
//! ```
//! # use pathscheme::*;
//! let matches = "/users/:id".parse::<PathScheme>()
//!     .unwrap()
//!     .matches("/users/olix0r")
//!     .unwrap();
//! assert!(matches.get("id").unwrap() == "olix0r");
//! ```
//!
//! Path schemes must match the entire path:
//!
//! ```
//! # use pathscheme::*;
//! assert!("/users/:id".parse::<PathScheme>()
//!     .unwrap()
//!     .matches("/users/olix0r/dogs")
//!     .is_none());
//!```
//!
//! A `**` glob operator may be used to match path prefixes:
//!
//! ```
//! # use pathscheme::*;
//! assert!("/users/:id/**".parse::<PathScheme>()
//!     .unwrap()
//!     .matches("/users/olix0r")
//!     .is_some());
//! assert!("/users/:id/**".parse::<PathScheme>()
//!     .unwrap()
//!     .matches("/users/olix0r/dogs")
//!     .is_some());
//!```

#![deny(warnings, rust_2018_idioms, missing_docs)]
#![forbid(unsafe_code)]
#![cfg_attr(docsrs, feature(doc_cfg))]

use indexmap::IndexMap;
use std::sync::Arc;

/// Describes a path scheme that may match one or paths.
#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct PathScheme {
    elements: Vec<Element>,
}

/// Indicates an error encountered when parsing a path scheme.
#[derive(Debug, PartialEq, thiserror::Error)]
#[non_exhaustive]
pub enum ParseError {
    /// Indicates an identifier, starting with a `:`, path component could not be read.
    #[error("path scheme includes invalid identifier: {0}")]
    InvalidIdentifier(String),

    /// Indicates a literal path component could not be read.
    #[error("path scheme includes invalid literal: {0}")]
    InvalidLiteral(String),

    /// Indicate a path scheme did not have a leading slash, e.g. `foo`.
    #[error("path scheme must start with a '/'")]
    RelativePath,

    /// Indicate a path scheme had a trailing slash, e.g. `/foo/`.
    #[error("path schemes must not include a trailing '/'")]
    TrailingSlash,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
enum Element {
    Literal(Arc<str>),
    Identifier(Arc<str>),
    SuffixGlob,
}

impl PathScheme {
    /// Matches the provided `path` against the scheme.
    ///
    /// Returns a value if the path matches the scheme, otherwise `None`. The
    /// returned map contains all identifiers expressed in the scheme.
    pub fn matches(&self, path: &str) -> Option<IndexMap<Arc<str>, String>> {
        let mut elements = self.elements.iter();
        let mut element = match elements.next() {
            Some(el) => el,
            None => return Some(IndexMap::default()),
        };

        let path = path.strip_prefix('/')?;
        let path = path.strip_suffix('/').unwrap_or(path);
        let mut paths = path.split('/');

        let mut matches = IndexMap::new();
        loop {
            match element {
                Element::Literal(p) => {
                    if paths.next()? != p.as_ref() {
                        return None;
                    }
                }
                Element::Identifier(id) => {
                    let segment = paths.next()?;
                    matches.insert(id.clone(), segment.to_string());
                }
                Element::SuffixGlob => {
                    break;
                }
            }

            element = match elements.next() {
                Some(el) => el,
                None => {
                    if paths.next().is_some() {
                        return None;
                    }
                    break;
                }
            };
        }

        Some(matches)
    }
}

impl std::str::FromStr for PathScheme {
    type Err = ParseError;

    fn from_str(path: &str) -> Result<Self, Self::Err> {
        let mut path = match path.split_once('/') {
            Some(("", path)) => path,
            _ => return Err(ParseError::RelativePath),
        };
        if path.ends_with('/') {
            return Err(ParseError::TrailingSlash);
        }

        let mut elements = Vec::new();
        while !path.is_empty() {
            if let Some(id) = path.strip_prefix(':') {
                if let Some((id, rest)) = id.split_once('/') {
                    if id.is_empty() {
                        return Err(ParseError::InvalidIdentifier(id.to_string()));
                    }
                    elements.push(Element::Identifier(Arc::from(id.to_string())));
                    path = rest;
                    continue;
                }

                if id.is_empty() {
                    return Err(ParseError::InvalidIdentifier(id.to_string()));
                }
                elements.push(Element::Identifier(Arc::from(id.to_string())));
                break;
            }

            if let Some((id, rest)) = path.split_once('/') {
                if id.is_empty() || id == "**" {
                    return Err(ParseError::InvalidLiteral(id.to_string()));
                }
                elements.push(Element::Literal(Arc::from(id.to_string())));
                path = rest;
                continue;
            }

            if path == "**" {
                elements.push(Element::SuffixGlob);
            } else {
                elements.push(Element::Literal(Arc::from(path.to_string())));
            }
            break;
        }

        Ok(PathScheme { elements })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty() {
        assert!(matches!(
            "".parse::<PathScheme>().unwrap_err(),
            ParseError::RelativePath
        ));
    }

    #[test]
    fn parse_slash() {
        assert_eq!(
            "/".parse::<PathScheme>().unwrap(),
            PathScheme { elements: vec![] }
        );
    }

    #[test]
    fn parse_slash_slash() {
        assert_eq!(
            "//".parse::<PathScheme>().unwrap_err(),
            ParseError::TrailingSlash
        );
        assert_eq!(
            "/foo//**".parse::<PathScheme>().unwrap_err(),
            ParseError::InvalidLiteral("".to_string())
        );
    }

    #[test]
    fn parse_absolute_path() {
        assert_eq!(
            "/foo/bar".parse::<PathScheme>().unwrap(),
            PathScheme {
                elements: vec![
                    Element::Literal(Arc::from("foo".to_string())),
                    Element::Literal(Arc::from("bar".to_string()))
                ]
            },
        );
        assert_eq!(
            "/foo/bar/".parse::<PathScheme>().unwrap_err(),
            ParseError::TrailingSlash,
        );
    }

    #[test]
    fn parse_relative_path() {
        assert!(matches!(
            "foo/bar".parse::<PathScheme>().unwrap_err(),
            ParseError::RelativePath
        ));
    }

    #[test]
    fn parse_identiifier() {
        assert_eq!(
            "/foo/:bar".parse::<PathScheme>().unwrap(),
            PathScheme {
                elements: vec![
                    Element::Literal(Arc::from("foo".to_string())),
                    Element::Identifier(Arc::from("bar".to_string()))
                ]
            },
        );
    }

    #[test]
    fn parse_suffix() {
        assert_eq!(
            "/foo/**".parse::<PathScheme>().unwrap(),
            PathScheme {
                elements: vec![
                    Element::Literal(Arc::from("foo".to_string())),
                    Element::SuffixGlob
                ]
            },
        );
        assert_eq!(
            "/foo/:bar/**".parse::<PathScheme>().unwrap(),
            PathScheme {
                elements: vec![
                    Element::Literal(Arc::from("foo".to_string())),
                    Element::Identifier(Arc::from("bar".to_string())),
                    Element::SuffixGlob
                ]
            },
        );
    }

    #[test]
    fn parse_empty_identifier() {
        assert_eq!(
            "/foo/:".parse::<PathScheme>().unwrap_err(),
            ParseError::InvalidIdentifier("".to_string())
        );
    }

    #[test]
    fn parse_suffix_must_be_last() {
        assert_eq!(
            "/foo/**/".parse::<PathScheme>().unwrap_err(),
            ParseError::TrailingSlash
        );
        assert_eq!(
            "/foo/**/bah".parse::<PathScheme>().unwrap_err(),
            ParseError::InvalidLiteral("**".to_string())
        );
    }

    #[test]
    fn matches_literal() {
        assert!("/".parse::<PathScheme>().unwrap().matches("/").is_some());
        assert!("/foo/bar"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/foo/bar/")
            .expect("matches")
            .is_empty());
        assert!("/foo/bar"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/foo/bar")
            .expect("matches")
            .is_empty());
        assert!("/foo/bar"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/foo/bar/bah")
            .is_none());
        assert!("/foo/bar/bah"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/foo/bar")
            .is_none());
    }

    #[test]
    fn matches_identifier() {
        let m = "/users/:id/face"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/users/olix0r/face")
            .expect("matches");
        assert_eq!(m.get("id").expect(":id matched"), "olix0r");
        let m = "/users/:id/**"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/users/olix0r/face/glasses")
            .expect("matches");
        assert_eq!(m.get("id").expect(":id matched"), "olix0r");
    }
}
