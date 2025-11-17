pub mod graph {
    use std::collections::HashMap;

    pub mod graph_items {
        pub mod node {
            use std::collections::HashMap;

            #[derive(Clone, Debug, PartialEq)]
            pub struct Node {
                pub name: String,
                attrs: HashMap<String, String>,
            }

            impl Node {
                pub fn new(name: &str) -> Self {
                    Self {
                        name: name.to_string(),
                        attrs: HashMap::new(),
                    }
                }

                pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
                    Self {
                        attrs: HashMap::from_iter(
                            attrs
                                .into_iter()
                                .map(|(k, v)| (k.to_string(), v.to_string())),
                        ),
                        ..self
                    }
                }

                pub fn attr(&self, k: &str) -> Option<&str> {
                    self.attrs.get(k).map(|v| v.as_str())
                }
            }
        }

        pub mod edge {
            use std::collections::HashMap;

            #[derive(Clone, Debug, PartialEq)]
            pub struct Edge {
                head: String,
                tail: String,
                attrs: HashMap<String, String>,
            }

            impl Edge {
                pub fn new(head: &str, tail: &str) -> Self {
                    Self {
                        head: head.to_string(),
                        tail: tail.to_string(),
                        attrs: HashMap::new(),
                    }
                }

                pub fn attr(&self, k: &str) -> Option<&str> {
                    self.attrs.get(k).map(|v| v.as_str())
                }

                pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
                    Self {
                        attrs: HashMap::from_iter(
                            attrs
                                .into_iter()
                                .map(|(k, v)| (k.to_string(), v.to_string())),
                        ),
                        ..self
                    }
                }
            }
        }
    }

    use graph_items::{edge::*, node::*};

    #[derive(Debug, PartialEq)]
    pub struct Graph {
        pub attrs: HashMap<String, String>,
        pub nodes: Vec<Node>,
        pub edges: Vec<Edge>,
    }

    impl Graph {
        pub fn new() -> Self {
            Self {
                attrs: HashMap::new(),
                nodes: Vec::new(),
                edges: Vec::new(),
            }
        }

        pub fn with_nodes(self, nodes: &[Node]) -> Self {
            Self {
                nodes: nodes.to_vec(),
                ..self
            }
        }

        pub fn with_edges(self, edges: &[Edge]) -> Self {
            Self {
                edges: edges.to_vec(),
                ..self
            }
        }

        pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
            Self {
                attrs: HashMap::from_iter(
                    attrs
                        .into_iter()
                        .map(|(k, v)| (k.to_string(), v.to_string())),
                ),
                ..self
            }
        }

        pub fn node(&self, name: &str) -> Result<&Node, String> {
            self.nodes
                .iter()
                .filter(|node| node.name == name)
                .next()
                .ok_or("Cannot find node with that name".to_string())
        }
    }
}
