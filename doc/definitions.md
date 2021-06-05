# Definitions

## Syntax

```
t :=    AtomicValues            (T-Atom)
        fn name: T. t           (T-Abs)
        t t                     (T-App)
        let id = t in t         (T-Let)
        let id: T = t in t      (T-LetCheck)
        if t then t else t      (T-If)
        loop t t                (T-Loop)
        (t,...,t)               (T-Tuple)
        TyName(t,...,t)         (T-NamedTuple)
        t.IntLiteral            (T-TupleAccess)
        TyName{ li = t, ..., lj = t } (T-Record)
        t.l                     (T-RecordAccess)
        unit                    (T-Unit)
        t as T                  (T-Cast)
        -t                      (T-Minus)
        t op t                  (T-BinaryOp)
    
AtomicValues := IntLiteral
                FloatLiteral
                BoolLiteral

v :=    unit
        AtomicValues
        TyName{ li = v, ..., lj = v}
        (v, ..., v)
        TyName(v, ..., v)

T :=    TyName
        (T, ..., T)
        T -> T
        Unit
        float
        int

TypeDeclare :=  type TyName(T,...,T)
                type Tyname{ li: T, ..., lj: T}

TopLevel := uniform name: T
            extern func_name: T
            TypDeclare
            let name = t
            let name: T = t

```

## Typing

```

```

## Evaluation

```
```