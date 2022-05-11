//! A basic path-matching language.
//!
//! ## Examples
//!
//! Path schemes may match literal paths:
//!
//! ```
//! # use pathscheme::*;
//! assert!("/users/new".parse::<PathScheme>()
//!     .expect("parses")
//!     .matches("/users/new")
//!     .expect("matches")
//!     .is_empty());
//! ```
//!
//! Literal path matching is case-sensitive:
//!
//! ```
//! # use pathscheme::*;
//! assert!("/users/new".parse::<PathScheme>()
//!     .expect("parses")
//!     .matches("/users/New")
//!     .is_none());
//! ```
//!
//! Or they may capture path segments:
//!
//! ```
//! # use pathscheme::*;
//! let matches = "/users/:id".parse::<PathScheme>()
//!     .expect("parses")
//!     .matches("/users/olix0r")
//!     .expect("matches");
//! assert!(matches.get("id").expect("id is set") == "olix0r");
//! ```
//!
//! Path schemes must match the entire path:
//!
//! ```
//! # use pathscheme::*;
//! assert!("/users/:id".parse::<PathScheme>()
//!     .expect("parses")
//!     .matches("/users/olix0r/dogs")
//!     .is_none());
//!```
//!
//! A `**` glob operator may be used to match path prefixes:
//!
//! ```
//! # use pathscheme::*;
//! assert!("/users/:id/**".parse::<PathScheme>()
//!     .expect("parses")
//!     .matches("/users/olix0r/dogs/bark")
//!     .is_some());
//! ```
//!
//! Suffix globs may also be named:
//!
//! ```
//! # use pathscheme::*;
//! let m = "/users/:id/*path".parse::<PathScheme>()
//!     .expect("parses")
//!     .matches("/users/olix0r/dogs/bark")
//!     .expect("matches");
//! assert_eq!(m.get("path").expect("path is set"), "dogs/bark");
//!```
//!
//! Named globs are only present in the resulting match map if the match is
//! non-empty:
//!
//! ```
//! # use pathscheme::*;
//! let m = "/users/:id/*path".parse::<PathScheme>()
//!     .expect("parses")
//!     .matches("/users/olix0r")
//!     .expect("matches");
//! assert!(m.get("path").is_none());
//! ```

#![deny(warnings, rust_2018_idioms, missing_docs)]
#![forbid(unsafe_code)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(feature = "serde")]
mod serde;

#[cfg(feature = "schemars")]
mod schemars;

use indexmap::IndexMap;
use std::{fmt::Write, sync::Arc};

/// Describes a path scheme that may match one or paths.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
enum Element {
    Literal(Arc<str>),
    Identifier(Arc<str>),
    SuffixGlob(Option<Arc<str>>),
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
                Element::SuffixGlob(Some(id)) => {
                    if let Some(p) = paths.next() {
                        let mut suffix = String::new();
                        suffix.push_str(p);
                        for p in paths {
                            suffix.push('/');
                            suffix.push_str(p);
                        }
                        matches.insert(id.clone(), suffix);
                    }
                    break;
                }
                Element::SuffixGlob(None) => break,
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
        debug_assert!(elements.next().is_none());

        Some(matches)
    }
}

impl std::str::FromStr for PathScheme {
    type Err = ParseError;

    fn from_str(path: &str) -> Result<Self, Self::Err> {
        let mut rest = match path.split_once('/') {
            Some(("", p)) if p.is_empty() => None,
            Some(("", p)) if p.ends_with('/') => return Err(ParseError::TrailingSlash),
            Some(("", p)) => Some(p),
            _ => return Err(ParseError::RelativePath),
        };

        let mut elements = Vec::new();
        while let Some(path) = rest {
            let component;
            (component, rest) = match path.split_once('/') {
                Some((id, path)) => (id, Some(path)),
                None => (path, None),
            };

            // If the next path component looks like an :identifier, parse it.
            if let Some(id) = component.strip_prefix(':') {
                // Add the identifier to the path (unless it's empty).
                if id.is_empty() {
                    return Err(ParseError::InvalidIdentifier(String::new()));
                }
                elements.push(Element::Identifier(Arc::from(id)));
            } else if let Some(glob) = component.strip_prefix('*') {
                if rest.is_some() {
                    return Err(ParseError::InvalidLiteral(format!("*{glob}")));
                }
                if glob == "*" {
                    elements.push(Element::SuffixGlob(None));
                } else {
                    elements.push(Element::SuffixGlob(Some(Arc::from(glob))));
                }
            } else {
                if component.is_empty() {
                    return Err(ParseError::InvalidLiteral(String::new()));
                }
                elements.push(Element::Literal(Arc::from(component)));
            }
        }

        Ok(PathScheme { elements })
    }
}

impl std::fmt::Display for PathScheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut elements = self.elements.iter();
        let mut element = match elements.next() {
            Some(el) => el,
            None => {
                f.write_char('/')?;
                return Ok(());
            }
        };
        loop {
            write!(f, "/{}", element)?;
            element = match elements.next() {
                Some(el) => el,
                None => return Ok(()),
            };
        }
    }
}

impl std::fmt::Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Element::Literal(p) => write!(f, "{p}")?,
            Element::Identifier(id) => write!(f, ":{id}")?,
            Element::SuffixGlob(Some(id)) => write!(f, "*{id}")?,
            Element::SuffixGlob(None) => write!(f, "**")?,
        }
        Ok(())
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
                    Element::Literal(Arc::from("foo")),
                    Element::Literal(Arc::from("bar"))
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
                    Element::Literal(Arc::from("foo")),
                    Element::Identifier(Arc::from("bar"))
                ]
            },
        );
    }

    #[test]
    fn parse_suffix() {
        assert_eq!(
            "/foo/*bars".parse::<PathScheme>().unwrap(),
            PathScheme {
                elements: vec![
                    Element::Literal(Arc::from("foo")),
                    Element::SuffixGlob(Some(Arc::from("bars")))
                ]
            },
        );
        assert_eq!(
            "/foo/:bar/*bahs".parse::<PathScheme>().unwrap(),
            PathScheme {
                elements: vec![
                    Element::Literal(Arc::from("foo")),
                    Element::Identifier(Arc::from("bar")),
                    Element::SuffixGlob(Some(Arc::from("bahs"))),
                ]
            },
        );
        assert_eq!(
            "/foo/**".parse::<PathScheme>().unwrap(),
            PathScheme {
                elements: vec![
                    Element::Literal(Arc::from("foo")),
                    Element::SuffixGlob(None)
                ]
            },
        );
        assert_eq!(
            "/foo/:bar/**".parse::<PathScheme>().unwrap(),
            PathScheme {
                elements: vec![
                    Element::Literal(Arc::from("foo")),
                    Element::Identifier(Arc::from("bar")),
                    Element::SuffixGlob(None),
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
            "/foo/*bars/".parse::<PathScheme>().unwrap_err(),
            ParseError::TrailingSlash
        );
        assert_eq!(
            "/foo/*bars/bah".parse::<PathScheme>().unwrap_err(),
            ParseError::InvalidLiteral("*bars".to_string())
        );
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
        let m = "/users/:id/*rest"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/users/olix0r/face/glasses")
            .expect("matches");
        assert_eq!(m.get("id").expect(":id matched"), "olix0r");
        assert_eq!(m.get("rest").expect("*rest matched"), "face/glasses");
    }

    #[test]
    fn matches_suffix() {
        assert!("/**"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/")
            .expect("matches")
            .is_empty());
        assert!("/**"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/users/olix0r/face")
            .expect("matches")
            .is_empty());
        let m = "/users/:id/face"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/users/olix0r/face")
            .expect("matches");
        assert_eq!(m.get("id").expect(":id matched"), "olix0r");
        let m = "/users/:id/*rest"
            .parse::<PathScheme>()
            .unwrap()
            .matches("/users/olix0r/face/glasses")
            .expect("matches");
        assert_eq!(m.get("id").expect(":id matched"), "olix0r");
        assert_eq!(m.get("rest").expect("*rest matched"), "face/glasses");
    }

    #[test]
    fn display() {
        for p in &["/", "/users/:id/face", "/users/:id/**", "/users/:id/*rest"] {
            assert_eq!(&p.parse::<PathScheme>().unwrap().to_string(), p);
        }
    }
}
