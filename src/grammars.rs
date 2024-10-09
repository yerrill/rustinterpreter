pub mod common {

    #[derive(Debug)]
    pub struct Variable(pub String);

    impl PartialEq for Variable {
        fn eq(&self, other: &Self) -> bool {
            let Variable(self_val) = self;
            let Variable(other_val) = other;
            *self_val == *other_val
        }
    }

    impl Eq for Variable {}

    impl Clone for Variable {
        fn clone(&self) -> Self {
            let Variable(value) = self;
            Variable((*value).clone())
        }
    }

    #[derive(Debug)]
    pub struct Constant(pub String);

    impl PartialEq for Constant {
        fn eq(&self, other: &Self) -> bool {
            let Constant(self_val) = self;
            let Constant(other_val) = other;
            *self_val == *other_val
        }
    }

    impl Eq for Constant {}

    impl Clone for Constant {
        fn clone(&self) -> Self {
            let Constant(value) = self;
            Constant((*value).clone())
        }
    }
}
