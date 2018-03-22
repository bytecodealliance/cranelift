//! Cretonne code generation library.

#![deny(missing_docs,
        trivial_numeric_casts,
        unused_extern_crates)]

#![cfg_attr(feature="clippy",
            plugin(clippy(conf_file="../../clippy.toml")))]
// Most of these are required for the generated code to pass clippy:
#![cfg_attr(feature="cargo-clippy",
            allow(new_without_default,
                  new_without_default_derive,
                  redundant_field_names,
                  many_single_char_names,
                  match_same_arms,
                  should_implement_trait,
                  identity_op,
                  needless_borrow,
                  cyclomatic_complexity,
                  too_many_arguments,
                  cast_lossless,
                  assign_op_pattern,
                  unreadable_literal,
                  empty_line_after_outer_attr,
// From here on i think theyre solvable:
                  match_wild_err_arm,
                  useless_let_if_seq,
                  len_without_is_empty,
                  single_match,
                  unused_lifetimes,
                  match_ref_pats,
                  iter_skip_next,
                  while_let_loop,
                  doc_markdown))]

pub use context::Context;
pub use legalizer::legalize_function;
pub use verifier::verify_function;
pub use write::write_function;

/// Version number of the cretonne crate.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[macro_use]
pub mod dbg;
#[macro_use]
pub mod entity;

pub mod bforest;
pub mod binemit;
pub mod cfg_printer;
pub mod cursor;
pub mod dominator_tree;
pub mod flowgraph;
pub mod ir;
pub mod isa;
pub mod loop_analysis;
pub mod packed_option;
pub mod print_errors;
pub mod result;
pub mod settings;
pub mod timing;
pub mod verifier;

mod abi;
mod bitset;
mod constant_hash;
mod context;
mod divconst_magic_numbers;
mod iterators;
mod legalizer;
mod licm;
mod partition_slice;
mod predicates;
mod preopt;
mod ref_slice;
mod regalloc;
mod scoped_hash_map;
mod simple_gvn;
mod stack_layout;
mod topo_order;
mod unreachable_code;
mod write;
