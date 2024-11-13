use std::borrow::Cow;
use rand::{Rng, RngCore};



/// 生成随机字符数据
pub trait SaltStrGenerator {
    fn gen_salt<'a>(self, num: usize) -> Cow<'a, str>;
    fn gen_salt_custom<'a>(self, num: usize, chars: Option<&str>) -> Cow<'a, str>;
}



const CHARS: &str = "abcdefghijklmnopqrstuvwxyz01234567890";

impl<T> SaltStrGenerator for T
where
    T: RngCore,
{
    fn gen_salt<'a>(self, num: usize) -> Cow<'a, str> {
        self.gen_salt_custom(num, None)
    }
    fn gen_salt_custom<'a>(
        mut self,
        num: usize,
        chars: Option<&str>,
    ) -> Cow<'a, str> {
        let chars = chars.unwrap_or(CHARS);
        let mut salt = String::new();
        for _ in 0..num {
            salt.push(chars.chars().nth(self.gen_range(0..chars.len())).unwrap());
        }
        Cow::Owned(salt)
    }
}