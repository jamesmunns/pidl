#[macro_export]
macro_rules! merrors {
    (
        | Name          | Error             | Label             | Help          |
        | $(-)*         | $(-)*             | $(-)*             | $(-)*         |
     $( | $name:ident   | $error:literal    | $label:literal    | $help:literal | )*
    ) => ($(
        #[derive(thiserror::Error, Debug, miette::Diagnostic)]
        #[error($error)]
        #[diagnostic(help($help))]
        pub struct $name {
            // The Source that we're gonna be printing snippets out of.
            // This can be a String if you don't have or care about file names.
            #[source_code]
            src: String,
            // Snippets and highlights can be included in the diagnostic!
            #[label($label)]
            bad_bit: miette::SourceSpan,
        }

        impl $name {
            pub fn new(src: &str, chunk: &str) -> Self {
                Self {
                    src: src.to_string(),
                    bad_bit: $crate::lexer::get_offset(src, chunk).unwrap().into(),
                }
            }

            pub fn from_input_spantoken(src: &str, stok: &$crate::lexer::SpanToken) -> Self {
                Self::new(
                    src,
                    &src[stok.span.0..][..stok.span.1],
                )
            }

            pub fn from_type_entry(src: &str, _tent: &$crate::TypeEntry) -> Self {
                Self::new(
                    src,
                    todo!(),
                )
            }
        }
    )*)
}

