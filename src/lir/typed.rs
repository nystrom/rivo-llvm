use super::trees::*;
use crate::mir::typed::Typed;

impl Typed for Exp {
    fn get_type(&self) -> Type {
        match self {
            Exp::FunctionAddr { ty, name } => ty.clone(),
            Exp::GlobalAddr { ty, name } => ty.clone(),
            Exp::Temp { ty, name } => ty.clone(),
            Exp::Lit { lit } => lit.get_type(),
        }
    }
}
