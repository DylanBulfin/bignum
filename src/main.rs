use bignumbe_rs::{
    create_default_base,
    traits::{Pred, Succ},
    BigNumBase,
};

//fn main() {
//    create_default_base!(Base3, 3);
//    type BigNum = BigNumBase<Base3>;
//
//    let mut start: BigNum = 1.into();
//    let mut end: BigNum = BigNum::new(1, u64::MAX / 2);
//
//    for _ in 1..100_000_000u64 {
//        let c = start * end;
//
//        if c > BigNum::new(1, u64::MAX / 4 * 3) {
//            panic!("Something has gone wrong")
//        }
//
//        start = start.succ();
//        end = end.pred();
//    }
//}

fn main() {}

//fn main() {
//    make_bignum!(3, BigNum);
//
//    let mut start: BigNum = 1.into();
//    let mut end: BigNum = BigNum::new(1, u64::MAX / 2);
//
//    for _ in 1..100_000_000u64 {
//        let c = start * end;
//
//        if c > BigNum::new(1, u64::MAX / 4 * 3) {
//            panic!("Something has gone wrong")
//        }
//
//        start = start.succ();
//        end = end.pred();
//    }
//}
