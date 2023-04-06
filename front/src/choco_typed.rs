use crate::{type_check::ChocoType, ast};

trait ChocoTyped {
    fn choco_type(&self) -> ChocoType;
}

impl ChocoTyped for ast::Expression<ChocoType> {
    fn choco_type(&self) -> ChocoType {
        todo!()
    }
}