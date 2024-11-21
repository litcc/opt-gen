mod opt_gen;
mod utils;

use proc_macro::TokenStream;

use crate::opt_gen::OptType;

#[proc_macro_derive(RawOpt, attributes(opt_gen))]
pub fn derive_raw_opt_model(item: TokenStream) -> TokenStream {
    opt_gen::opt_gen(item, OptType::Raw)
}

#[proc_macro_derive(CowOpt, attributes(opt_gen))]
pub fn derive_cow_opt_model(item: TokenStream) -> TokenStream {
    opt_gen::opt_gen(item, OptType::Cow)
}
