use crate::hir::trees as hir;
use crate::mir::trees as mir;
use crate::hir::ops::*;
use crate::common::names::*;
use super::runtime_api as api;

pub struct Translate;

impl Translate {
    pub fn translate(r: &hir::Root) -> mir::Root {
        // Lambda lift
        use crate::hir::cc::*;
        let cc = LambdaLift::lambda_lift(&r);

        let procs = cc.defs.iter().flat_map(|p| match p {
            hir::Def::FunDef { ret_type, name, params, body } =>
                Some(ProcTranslator::new(Translate::translate_type(ret_type)).translate(ret_type, *name, params, &**body)),
            _ => None,
        }).collect();

        mir::Root { data: vec![], procs }
    }

    pub fn translate_type(ty: &hir::Type) -> mir::Type {
        match ty {
            hir::Type::I32 => mir::Type::I32,
            hir::Type::I64 => mir::Type::I64,
            hir::Type::F32 => mir::Type::F32,
            hir::Type::F64 => mir::Type::F64,
            hir::Type::Bool => mir::Type::I1,
            hir::Type::Void => mir::Type::Void,
            hir::Type::Array { ty } => mir::Type::Array {
                ty: Box::new(Translate::translate_type(ty))
            },
            hir::Type::Struct { fields } => mir::Type::Struct {
                fields: fields.iter().map(|f| Translate::translate_type(&f.ty)).collect()
            },

            // TODO
            hir::Type::Fun { ret, args } => mir::Type::Fun {
                ret: Box::new(Translate::translate_type(ret)),
                args: args.iter().map(|ty| Translate::translate_type(ty)).collect()
            },
            hir::Type::Box => mir::Type::Struct { fields: vec![] }, // TODO: void*
        }
    }
}

struct ProcTranslator {
}

impl ProcTranslator {
    fn new(ty: mir::Type) -> Self {
        ProcTranslator {
        }
    }

    fn new_temp(&mut self) -> Name {
        Name::fresh("t.mir")
    }

    fn new_label(&mut self) -> Name {
        Name::fresh("L.mir")
    }

    fn translate(&mut self, ty: &hir::Type, name: Name, params: &Vec<hir::Param>, body: &hir::Exp) -> mir::Proc {
        let mir_body = self.translate_exp(body);
        let mir_ty = Translate::translate_type(ty);

        let mir_params = params.iter().map(|param|
            mir::Param { name: param.name, ty: Translate::translate_type(&param.ty) }
        ).collect();

        mir::Proc {
            ret_type: mir_ty,
            name,
            params: mir_params,
            body: Box::new(mir_body)
        }
    }

    fn get_field_index(struct_ty: &hir::Type, field: Name) -> (usize, hir::Type) {
        match struct_ty {
            hir::Type::Struct { fields } => {
                for (i, f) in fields.iter().enumerate() {
                    match f {
                        hir::Param { ty, name } => {
                            if *name == field {
                                return (i, ty.clone())
                            }
                        }
                    }
                }
            },
            _ => {},
        }

        panic!("invalid field name {} for type {:?}", field, struct_ty)
    }

    fn translate_stm(&mut self, stm: &hir::Stm) -> Vec<mir::Stm> {
        match stm {
            hir::Stm::Eval { exp } => {
                // Just assign into a fresh temp and rely on regalloc to eliminate.
                let t = self.new_temp();
                vec![
                    mir::Stm::Move {
                        ty: mir::Type::Void,
                        lhs: t,
                        rhs: Box::new(self.translate_exp(&*exp))
                    }
                ]
            },
            hir::Stm::Assign { ty, lhs, rhs } => {
                vec![
                    mir::Stm::Move {
                        ty: Translate::translate_type(ty),
                        lhs: *lhs,
                        rhs: Box::new(self.translate_exp(&*rhs))
                    }
                ]
            },
            // To simplify things, handle three cases recursively to add the bounds check.
            hir::Stm::ArrayAssign { bounds_check: true, ty, array: array @ box hir::Exp::Var { .. }, index: index @ box hir::Exp::Var { .. }, value } => {
                let assign = hir::Stm::ArrayAssign {
                    bounds_check: false,
                    ty: ty.clone(),
                    array: array.clone(),
                    index: index.clone(),
                    value: value.clone()
                };

                let check = hir::Exp::Binary {
                    op: Bop::Lt_u_i32,
                    e1: index.clone(),
                    e2: Box::new(hir::Exp::ArrayLength { array: array.clone() }),
                };

                let ite = hir::Stm::IfElse {
                    cond: Box::new(check),
                    if_true: Box::new(assign),
                    if_false: Box::new(
                        hir::Stm::Eval {
                            exp: Box::new(
                                hir::Exp::Call {
                                    fun_type: hir::Type::Fun { ret: Box::new(hir::Type::Void), args: vec![] },
                                    name: Name::new("panic"),
                                    args: vec![]
                                }
                            )
                        }
                    ),
                };

                self.translate_stm(&ite)
            },
            hir::Stm::ArrayAssign { bounds_check: true, ty, array, index, value } => {
                let a = self.new_temp();
                let i = self.new_temp();

                let assign = hir::Exp::Let {
                    inits: vec![
                        hir::Field {
                            param: hir::Param {
                                ty: hir::Type::Array { ty: Box::new(ty.clone()) },
                                name: a,
                            },
                            exp: array.clone(),
                        },
                        hir::Field {
                            param: hir::Param {
                                ty: hir::Type::I32,
                                name: i,
                            },
                            exp: index.clone(),
                        }
                    ],
                    body: Box::new(
                        hir::Exp::Seq {
                            body: Box::new(
                                hir::Stm::ArrayAssign {
                                    bounds_check: true,
                                    ty: ty.clone(),
                                    array: Box::new(hir::Exp::Var { ty: hir::Type::Array { ty: Box::new(ty.clone()) }, name: a }),
                                    index: Box::new(hir::Exp::Var { ty: hir::Type::I32, name: i }),
                                    value: value.clone()
                                }
                            ),
                            // Just eval to false. We'll discard this value.
                            exp: Box::new(hir::Exp::Lit { lit: hir::Lit::Bool { value: false } })
                        }
                    )
                };

                self.translate_stm(&hir::Stm::Eval { exp: Box::new(assign) })
            },
            hir::Stm::ArrayAssign { bounds_check: false, ty, array, index, value } => {
                let base_ty = Translate::translate_type(ty);
                let a = self.translate_exp(&*array);
                let i = self.translate_exp(&*index);
                let v = self.translate_exp(&*value);

                vec![
                    mir::Stm::Store {
                        ty: base_ty.clone(),
                        ptr: Box::new(
                            mir::Exp::GetArrayElementAddr {
                                base_ty: base_ty.clone(),
                                ptr: Box::new(a),
                                index: Box::new(i),
                            }
                        ),
                        value: Box::new(v)
                    }
                ]
            },
            hir::Stm::StructAssign { ty, base, field, value } => {
                let struct_ty = Translate::translate_type(ty);

                let (i, hfield_ty) = ProcTranslator::get_field_index(ty, *field);
                let field_ty = Translate::translate_type(&hfield_ty);

                let p = self.translate_exp(&*base);
                let v = self.translate_exp(&*value);

                vec![
                    mir::Stm::Store {
                        ty: field_ty,
                        ptr: Box::new(
                            mir::Exp::GetStructElementAddr {
                                struct_ty: struct_ty.clone(),
                                ptr: Box::new(p),
                                field: i
                            }
                        ),
                        value: Box::new(v)
                    }
                ]
            },
            hir::Stm::Return { exp } => {
                vec![
                    mir::Stm::Return {
                        exp: Box::new(self.translate_exp(&*exp))
                    },
                ]
            },
            hir::Stm::IfElse { cond, if_true, if_false } => {
                let bottom = self.new_label();
                let l_if_true = self.new_label();
                let l_if_false = self.new_label();
                let e = self.translate_exp(&*cond);
                let mut m1 = self.translate_stm(&*if_true);
                let mut m2 = self.translate_stm(&*if_false);

                let mut stms = Vec::new();
                stms.push(mir::Stm::CJump {
                    cond: Box::new(e),
                    if_true: l_if_true,
                    if_false: l_if_false
                });
                stms.push(mir::Stm::Label { label: l_if_true });
                stms.append(&mut m1);
                stms.push(mir::Stm::Jump { label: bottom });
                stms.push(mir::Stm::Label { label: l_if_false });
                stms.append(&mut m2);
                stms.push(mir::Stm::Jump { label: bottom });
                stms.push(mir::Stm::Label { label: bottom });
                stms
            },
            hir::Stm::IfThen { cond, if_true } => {
                let bottom = self.new_label();
                let l_if_true = self.new_label();
                let e = self.translate_exp(&*cond);
                let mut m = self.translate_stm(&*if_true);

                let mut stms = Vec::new();
                stms.push(mir::Stm::CJump {
                    cond: Box::new(e),
                    if_true: l_if_true,
                    if_false: bottom
                });
                stms.push(mir::Stm::Label { label: l_if_true });
                stms.append(&mut m);
                stms.push(mir::Stm::Jump { label: bottom });
                stms.push(mir::Stm::Label { label: bottom });
                stms
            },
            hir::Stm::While { cond, body } => {
                let bottom = self.new_label();
                let top = self.new_label();
                let l_body = self.new_label();
                let e = self.translate_exp(&*cond);
                let mut m = self.translate_stm(&*body);

                let mut stms = Vec::new();
                stms.push(mir::Stm::Label { label: top });
                stms.push(mir::Stm::CJump {
                    cond: Box::new(e),
                    if_true: l_body,
                    if_false: bottom
                });
                stms.push(mir::Stm::Label { label: l_body });
                stms.append(&mut m);
                stms.push(mir::Stm::Jump { label: top });
                stms.push(mir::Stm::Label { label: bottom });
                stms
            },
            hir::Stm::Block { body } => {
                body.iter().flat_map(|s| self.translate_stm(s)).collect()
            },
        }
    }

    fn nonzero(&mut self, ty: mir::Type, e: mir::Exp) -> mir::Exp {
        // If e is definitely not zero, just return e.
        match e {
            mir::Exp::Lit { lit: mir::Lit::I32 { value: n } } if n != 0 => { return e; },
            mir::Exp::Lit { lit: mir::Lit::I64 { value: n } } if n != 0 => { return e; },
            _ => {},
        }

        let if_true = self.new_label();
        let if_false = self.new_label();
        let t = self.new_temp();
        let scratch = self.new_temp();
        let cmp = match ty {
            mir::Type::I32 => Bop::Eq_i32,
            mir::Type::I64 => Bop::Eq_i64,
            _ => panic!("unexpected integer type"),
        };
        let zero = match ty {
            mir::Type::I32 => mir::Lit::I32 { value: 0 },
            mir::Type::I64 => mir::Lit::I64 { value: 0 },
            _ => panic!("unexpected integer type"),
        };

        mir::Exp::Block {
            body: vec![
                mir::Stm::Move { ty: ty.clone(), lhs: t, rhs: Box::new(e) },
                mir::Stm::CJump {
                    cond: Box::new(
                        mir::Exp::Binary {
                            op: cmp,
                            e1: Box::new(mir::Exp::Temp { ty: ty.clone(), name: t }),
                            e2: Box::new(mir::Exp::Lit { lit: zero }),
                        }
                    ),
                    if_true,
                    if_false,
                },
                mir::Stm::Label { label: if_true },
                mir::Stm::Move {
                    ty: mir::Type::Void,
                    lhs: scratch,
                    rhs: Box::new(
                        mir::Exp::Call {
                            fun_type: mir::Type::Fun { ret: Box::new(mir::Type::Void), args: vec![] },
                            fun: Box::new(api::panic()),
                            args: vec![]
                        }
                    )
                },
                mir::Stm::Label { label: if_false },
                mir::Stm::Nop,
            ],
            exp: Box::new(mir::Exp::Temp { ty, name: t })
        }
    }

    fn simplify(e: &hir::Exp) -> &hir::Exp {
        match &e {
            // box(unbox(e)) == e
            hir::Exp::Box { ty: boxed, exp } => match &**exp {
                hir::Exp::Unbox { ty: unboxed, exp } => {
                    if boxed == unboxed {
                        &**exp
                    }
                    else {
                        e
                    }
                },
                _ => e
            }

            // unbox(box(e)) == e
            hir::Exp::Unbox { ty: unboxed, exp } => match &**exp {
                hir::Exp::Box { ty: boxed, exp } => {
                    if boxed == unboxed {
                        &**exp
                    }
                    else {
                        e
                    }
                },
                _ => e
            }

            // For integers only:
            // e1 + -e2 == e1 - e2
            // e1 + 0 == e1
            // 0 + e2 == e2
            // e1 * 1 == e1
            // 1 * e2 == e2

            e => e
        }
    }

    fn translate_exp(&mut self, e: &hir::Exp) -> mir::Exp {
        let simplified_exp = ProcTranslator::simplify(e);

        match simplified_exp {
            hir::Exp::NewArray { ty, length } => {
                let len = self.new_temp();
                let array = self.new_temp();

                let n = self.translate_exp(&*length);

                let base_ty = Translate::translate_type(ty);

                let array_type = mir::Type::Array {
                    ty: Box::new(base_ty.clone())
                };

                // To allocate, we compute the size of the array, then call malloc.
                let alloc = mir::Exp::Call {
                    fun_type: mir::Type::Fun { ret: Box::new(array_type.clone()), args: vec![mir::Type::Word] },
                    fun: Box::new(api::alloc()),
                    args: vec![
                        mir::Exp::Binary {
                            op: Bop::Add_word,
                            e1: Box::new(mir::Exp::Lit { lit: mir::Lit::ArrayBaseOffset }),
                            e2: Box::new(
                                mir::Exp::Binary {
                                    op: Bop::Mul_word,
                                    e1: Box::new(mir::Exp::Lit { lit: mir::Lit::Sizeof { ty: base_ty.clone() }}),
                                    e2: Box::new(mir::Exp::Temp { name: len, ty: mir::Type::Word }),
                                }
                            ),
                        }
                    ]
                };

                mir::Exp::Block {
                    body: vec![
                        mir::Stm::Move { ty: mir::Type::Word, lhs: len, rhs: Box::new(n) },
                        mir::Stm::Move { ty: mir::Type::Ptr { ty: Box::new(array_type.clone()) }, lhs: array, rhs: Box::new(alloc) },
                        mir::Stm::Store {
                            ty: mir::Type::Word,
                            ptr: Box::new(
                                mir::Exp::GetArrayLengthAddr {
                                    ptr: Box::new(mir::Exp::Temp { name: array, ty: array_type.clone() })
                                }
                            ),
                            value: Box::new(mir::Exp::Temp { name: len, ty: mir::Type::Word }),
                        }
                    ],
                    exp: Box::new(mir::Exp::Temp { name: array, ty: array_type.clone() })
                }
            },

            hir::Exp::ArrayLit { ty, exps } => {
                // Do new array, then assign into the array.
                // Or memcpy if expressions are all literals.
                let new_array = hir::Exp::NewArray { ty: ty.clone(), length: Box::new(hir::Exp::Lit { lit: hir::Lit::I32 { value: exps.len() as i32 } }) };
                let array_type = hir::Type::Array { ty: Box::new(ty.clone()) };
                let t = self.new_temp();
                let array_var = hir::Exp::Var { ty: array_type.clone(), name: t };

                let mut inits = Vec::new();

                for (i, e) in exps.iter().enumerate() {
                    inits.push(
                        hir::Stm::ArrayAssign {
                            bounds_check: false,
                            ty: ty.clone(),
                            array: Box::new(array_var.clone()),
                            index: Box::new(hir::Exp::Lit { lit: hir::Lit::I32 { value: i as i32 } }),
                            value: Box::new(e.clone()),
                        }
                    );
                }

                let init = hir::Exp::Let {
                    inits: vec![
                        hir::Field {
                            param: hir::Param {
                                ty: array_type.clone(),
                                name: t,
                            },
                            exp: Box::new(new_array),
                        }
                    ],
                    body: Box::new(
                        hir::Exp::Seq {
                            body: Box::new(hir::Stm::Block { body: inits }),
                            exp: Box::new(array_var)
                        }
                    )
                };

                self.translate_exp(&init)
            }
            hir::Exp::StructLit { fields } => {
                let field_types: Vec<mir::Type> = fields.iter().map(|f| Translate::translate_type(&f.param.ty)).collect();
                let struct_ty = mir::Type::Struct { fields: field_types.clone() };

                let alloc = mir::Exp::Call {
                    fun_type: mir::Type::Fun { ret: Box::new(struct_ty.clone()), args: vec![mir::Type::Word] },
                    fun: Box::new(api::alloc()),
                    args: vec![
                        mir::Exp::Lit { lit: mir::Lit::Sizeof { ty: struct_ty.clone() } },
                    ],
                };

                let t = self.new_temp();

                let mut ss = Vec::new();

                // t = malloc(sizeof(ty))
                ss.push(
                    mir::Stm::Move {
                        ty: struct_ty.clone(),
                        lhs: t,
                        rhs: Box::new(alloc),
                    }
                );

                let p = mir::Exp::Temp { name: t, ty: struct_ty.clone() };

                for (i, (field, field_ty)) in fields.iter().zip(field_types.iter()).enumerate() {
                    let v = self.translate_exp(&*field.exp);

                    ss.push(
                        mir::Stm::Store {
                            ty: field_ty.clone(),
                            ptr: Box::new(
                                mir::Exp::GetStructElementAddr {
                                    struct_ty: struct_ty.clone(),
                                    ptr: Box::new(p.clone()),
                                    field: i
                                }
                            ),
                            value: Box::new(v)
                        }
                    );
                }

                mir::Exp::Block {
                    body: ss,
                    exp: Box::new(p)
                }
            },
            hir::Exp::Lambda { ret_type, params, body } => {
                // These shouldn't exist after lambda lifting.
                unimplemented!()
            },
            hir::Exp::Apply { fun_type, fun, args } => {
                mir::Exp::Call {
                    fun_type: Translate::translate_type(fun_type),
                    fun: Box::new(self.translate_exp(&*fun)),
                    args: args.iter().map(|e| self.translate_exp(e)).collect(),
                }
            },
            hir::Exp::Call { fun_type, name, args } => {
                let ty = Translate::translate_type(fun_type);
                mir::Exp::Call {
                    fun_type: ty.clone(),
                    fun: Box::new(mir::Exp::Function { name: *name, ty: ty.clone() }),
                    args: args.iter().map(|e| self.translate_exp(e)).collect(),
                }
            },
            hir::Exp::Var { name, ty } => {
                mir::Exp::Temp { name: *name, ty: Translate::translate_type(ty) }
            },

            hir::Exp::ArrayLoad { bounds_check: true, ty, array, index } => {
                // TODO: bounds check
                self.translate_exp(
                    &hir::Exp::ArrayLoad {
                        bounds_check: false,
                        ty: ty.clone(),
                        array: array.clone(),
                        index: index.clone()
                    }
                )
            },
            hir::Exp::ArrayLoad { bounds_check: false, ty, array, index } => {
                let base_ty = Translate::translate_type(ty);
                let a = self.translate_exp(&*array);
                let i = self.translate_exp(&*index);

                mir::Exp::Load {
                    ty: base_ty.clone(),
                    ptr: Box::new(
                        mir::Exp::GetArrayElementAddr {
                            base_ty: base_ty.clone(),
                            ptr: Box::new(a),
                            index: Box::new(i),
                        }
                    ),
                }
            },
            hir::Exp::StructLoad { ty, base, field } => {
                let struct_ty = Translate::translate_type(ty);

                let (i, hfield_ty) = ProcTranslator::get_field_index(ty, *field);
                let field_ty = Translate::translate_type(&hfield_ty);

                let p = self.translate_exp(&*base);

                mir::Exp::Load {
                    ty: field_ty,
                    ptr: Box::new(
                        // Address of a struct field entry.
                        mir::Exp::GetStructElementAddr {
                            struct_ty: struct_ty.clone(),
                            ptr: Box::new(p),
                            field: i
                        }
                    ),
                }
            },
            hir::Exp::ArrayLength { array } => {
                let a = self.translate_exp(&*array);

                mir::Exp::Load {
                    ty: mir::Type::Word,
                    ptr: Box::new(
                        mir::Exp::GetArrayLengthAddr { ptr: Box::new(a) }
                    )
                }
            }

            hir::Exp::Lit { lit: hir::Lit::I32 { value } } => mir::Exp::Lit { lit: mir::Lit::I32 { value: *value } },
            hir::Exp::Lit { lit: hir::Lit::I64 { value } } => mir::Exp::Lit { lit: mir::Lit::I64 { value: *value } },
            hir::Exp::Lit { lit: hir::Lit::F32 { value } } => mir::Exp::Lit { lit: mir::Lit::F32 { value: *value } },
            hir::Exp::Lit { lit: hir::Lit::F64 { value } } => mir::Exp::Lit { lit: mir::Lit::F64 { value: *value } },

            // Booleans are implemented as i1.
            hir::Exp::Lit { lit: hir::Lit::Bool { value: false } } => mir::Exp::Lit { lit: mir::Lit::I1 { value: false } },
            hir::Exp::Lit { lit: hir::Lit::Bool { value: true } } => mir::Exp::Lit { lit: mir::Lit::I1 { value: true } },

            // Short-circuiting &&
            hir::Exp::Binary { op: Bop::And_z, e1, e2 } => {
                // e1 && e2 == if e1 then e2 else false
                let t = self.new_temp();
                let bottom = self.new_label();
                let if_true = self.new_label();
                let m1 = self.translate_exp(&*e1);
                let m2 = self.translate_exp(&*e2);

                mir::Exp::Block {
                    body: vec![
                        // t = false;
                        mir::Stm::Move {
                            ty: mir::Type::I1,
                            lhs: t,
                            rhs: Box::new(mir::Exp::Lit { lit: mir::Lit::I1 { value: false }}),
                        },
                        // if e1 goto T else Bot
                        mir::Stm::CJump {
                            cond: Box::new(m1),
                            if_true: if_true,
                            if_false: bottom,
                        },
                        // T:
                        mir::Stm::Label { label: if_true },
                        // t = e2;
                        mir::Stm::Move {
                            ty: mir::Type::I1,
                            lhs: t,
                            rhs: Box::new(m2)
                        },
                        // Bot:
                        mir::Stm::Label { label: bottom },
                    ],
                    exp: Box::new(mir::Exp::Temp { name: t, ty: mir::Type::I1 })
                }
            },
            // Short-circuiting ||
            hir::Exp::Binary { op: Bop::Or_z, e1, e2 } => {
                // e1 || e2 == if e1 then true else e2
                let t = self.new_temp();
                let bottom = self.new_label();
                let if_false = self.new_label();
                let m1 = self.translate_exp(&*e1);
                let m2 = self.translate_exp(&*e2);

                mir::Exp::Block {
                    body: vec![
                        // t = true;
                        mir::Stm::Move {
                            ty: mir::Type::I1,
                            lhs: t,
                            rhs: Box::new(mir::Exp::Lit { lit: mir::Lit::I1 { value: true } }),
                        },
                        // if e1 goto Bot else F
                        mir::Stm::CJump {
                            cond: Box::new(m1),
                            if_true: bottom,
                            if_false: if_false,
                        },
                        // F:
                        mir::Stm::Label { label: if_false },
                        // t = e2
                        mir::Stm::Move {
                            ty: mir::Type::I1,
                            lhs: t,
                            rhs: Box::new(m2)
                        },
                        // Bot:
                        mir::Stm::Label { label: bottom },
                    ],
                    exp: Box::new(mir::Exp::Temp { name: t, ty: mir::Type::I1 })
                }
            },

            // Div and rem need to check for 0.
            hir::Exp::Binary { op: op @ Bop::Div_s_i32, e1, e2 } => mir::Exp::Binary {
                op: *op,
                e1: Box::new(self.translate_exp(&*e1)),
                e2: { let e = self.translate_exp(&*e2); Box::new(self.nonzero(mir::Type::I32, e)) },
            },
            hir::Exp::Binary { op: op @ Bop::Div_s_i64, e1, e2 } => mir::Exp::Binary {
                op: *op,
                e1: Box::new(self.translate_exp(&*e1)),
                e2: { let e = self.translate_exp(&*e2); Box::new(self.nonzero(mir::Type::I64, e)) },
            },
            hir::Exp::Binary { op: op @ Bop::Div_u_i32, e1, e2 } => mir::Exp::Binary {
                op: *op,
                e1: Box::new(self.translate_exp(&*e1)),
                e2: { let e = self.translate_exp(&*e2); Box::new(self.nonzero(mir::Type::I32, e)) },
            },
            hir::Exp::Binary { op: op @ Bop::Div_u_i64, e1, e2 } => mir::Exp::Binary {
                op: *op,
                e1: Box::new(self.translate_exp(&*e1)),
                e2: { let e = self.translate_exp(&*e2); Box::new(self.nonzero(mir::Type::I64, e)) },
            },
            hir::Exp::Binary { op: op @ Bop::Rem_s_i32, e1, e2 } => mir::Exp::Binary {
                op: *op,
                e1: Box::new(self.translate_exp(&*e1)),
                e2: { let e = self.translate_exp(&*e2); Box::new(self.nonzero(mir::Type::I32, e)) },
            },
            hir::Exp::Binary { op: op @ Bop::Rem_s_i64, e1, e2 } => mir::Exp::Binary {
                op: *op,
                e1: Box::new(self.translate_exp(&*e1)),
                e2: { let e = self.translate_exp(&*e2); Box::new(self.nonzero(mir::Type::I64, e)) },
            },
            hir::Exp::Binary { op: op @ Bop::Rem_u_i32, e1, e2 } => mir::Exp::Binary {
                op: *op,
                e1: Box::new(self.translate_exp(&*e1)),
                e2: { let e = self.translate_exp(&*e2); Box::new(self.nonzero(mir::Type::I32, e)) },
            },
            hir::Exp::Binary { op: op @ Bop::Rem_u_i64, e1, e2 } => mir::Exp::Binary {
                op: *op,
                e1: Box::new(self.translate_exp(&*e1)),
                e2: { let e = self.translate_exp(&*e2); Box::new(self.nonzero(mir::Type::I64, e)) },
            },

            // Other binary operations are just translated as is.
            hir::Exp::Binary { op, e1, e2 } => mir::Exp::Binary {
                op: *op,
                e1: Box::new(self.translate_exp(&*e1)),
                e2: Box::new(self.translate_exp(&*e2))
            },

            // Other unary operations are just translated as is.
            hir::Exp::Unary { op, exp } => {
                mir::Exp::Unary {
                    op: *op,
                    exp: Box::new(self.translate_exp(&*exp))
                }
            },

            // Boxing and unboxing
            hir::Exp::Box { ty, exp } => {
                use crate::mir::typed::Typed;
                let mir_ty = Translate::translate_type(&ty);
                let arg = self.translate_exp(&*exp);
                mir::Exp::Call {
                    fun_type: mir::Type::Fun { ret: Box::new(Translate::translate_type(&hir::Type::Box)), args: vec![mir_ty.clone()] },
                    fun: Box::new(api::boxer(&mir_ty)),
                    args: vec![arg]
                }
            },
            hir::Exp::Unbox { ty, exp } => {
                use crate::mir::typed::Typed;
                let mir_ty = Translate::translate_type(&ty);
                let arg = self.translate_exp(&*exp);
                mir::Exp::Call {
                    fun_type: mir::Type::Fun { ret: Box::new(mir_ty.clone()), args: vec![Translate::translate_type(&hir::Type::Box)] },
                    fun: Box::new(api::unboxer(&mir_ty)),
                    args: vec![arg]
                }
            },
            hir::Exp::Cast { ty, exp } => {
                let mir_ty = Translate::translate_type(&ty);
                mir::Exp::Cast { ty: mir_ty, exp: Box::new(self.translate_exp(&*exp)) }
            },

            hir::Exp::Seq { body, exp } => {
                let mir_body = self.translate_stm(&*body);
                let mir_exp = self.translate_exp(&*exp);
                mir::Exp::Block {
                    body: mir_body,
                    exp: Box::new(mir_exp),
                }
            },
            hir::Exp::Let { inits, body } => {
                let mir_inits = inits.iter().map(|init| {
                    let e = self.translate_exp(&*init.exp);
                    mir::Stm::Move {
                        ty: Translate::translate_type(&init.param.ty),
                        lhs: init.param.name,
                        rhs: Box::new(e)
                    }
                }).collect();
                let mir_body = self.translate_exp(&*body);
                mir::Exp::Block {
                    body: mir_inits,
                    exp: Box::new(mir_body),
                }
            }
        }
    }
}
