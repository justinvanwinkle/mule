; Core LLVM intrinsics and singleton descriptors (`@mule.true`, `@mule.false`, `@mule.none`, `@mule.undefined`).

declare {i64, i1} @llvm.sadd.with.overflow.i64(i64, i64)
declare {i64, i1} @llvm.ssub.with.overflow.i64(i64, i64)
declare {i64, i1} @llvm.smul.with.overflow.i64(i64, i64)
declare double @llvm.sqrt.f64(double)
@mule.true = external global i8, align 8
@mule.false = external global i8, align 8
@mule.none = external global i8, align 8
@mule.undefined = external global i8, align 8
