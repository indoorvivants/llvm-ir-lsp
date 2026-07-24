; ModuleID = '/app/example.c'
source_filename = "/app/example.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @func(ptr noundef %0) #0 !dbg !10 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !18, metadata !DIExpression()), !dbg !19
  %3 = load ptr, ptr %2, align 8, !dbg !20
  %4 = load float, ptr %3, align 4, !dbg !21
  %5 = fpext float %4 to double, !dbg !22
  %6 = fcmp ogt double %5, 2.500000e+01, !dbg !23
  ret i1 %6, !dbg !24
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @square(i32 noundef %0) #0 !dbg !25 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !29, metadata !DIExpression()), !dbg !30
  %3 = load i32, ptr %2, align 4, !dbg !31
  %4 = load i32, ptr %2, align 4, !dbg !32
  %5 = mul nsw i32 %3, %4, !dbg !33
  ret i32 %5, !dbg !34
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5, !6, !7, !8}
!llvm.ident = !{!9}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 15.0.0 (https://github.com/llvm/llvm-project.git 4ba6a9c9f65bbc8bd06e3652cb20fd4dfc846137)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/app/example.c", directory: "/app")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !{i32 1, !"wchar_size", i32 4}
!5 = !{i32 7, !"PIC Level", i32 2}
!6 = !{i32 7, !"PIE Level", i32 2}
!7 = !{i32 7, !"uwtable", i32 2}
!8 = !{i32 7, !"frame-pointer", i32 2}
!9 = !{!"clang version 15.0.0 (https://github.com/llvm/llvm-project.git 4ba6a9c9f65bbc8bd06e3652cb20fd4dfc846137)"}
!10 = distinct !DISubprogram(name: "func", scope: !11, file: !11, line: 4, type: !12, scopeLine: 4, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!11 = !DIFile(filename: "example.c", directory: "/app")
!12 = !DISubroutineType(types: !13)
!13 = !{!14, !15}
!14 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!15 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !16, size: 64)
!16 = !DIBasicType(name: "float", size: 32, encoding: DW_ATE_float)
!17 = !{}
!18 = !DILocalVariable(name: "param", arg: 1, scope: !10, file: !11, line: 4, type: !15)
!19 = !DILocation(line: 4, column: 18, scope: !10)
!20 = !DILocation(line: 5, column: 14, scope: !10)
!21 = !DILocation(line: 5, column: 13, scope: !10)
!22 = !DILocation(line: 5, column: 12, scope: !10)
!23 = !DILocation(line: 5, column: 21, scope: !10)
!24 = !DILocation(line: 5, column: 5, scope: !10)
!25 = distinct !DISubprogram(name: "square", scope: !11, file: !11, line: 8, type: !26, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!26 = !DISubroutineType(types: !27)
!27 = !{!28, !28}
!28 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!29 = !DILocalVariable(name: "num", arg: 1, scope: !25, file: !11, line: 8, type: !28)
!30 = !DILocation(line: 8, column: 16, scope: !25)
!31 = !DILocation(line: 9, column: 12, scope: !25)
!32 = !DILocation(line: 9, column: 18, scope: !25)
!33 = !DILocation(line: 9, column: 16, scope: !25)
!34 = !DILocation(line: 9, column: 5, scope: !25)
