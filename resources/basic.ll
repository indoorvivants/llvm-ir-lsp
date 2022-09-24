; ModuleID = '/app/example.cpp'
source_filename = "/app/example.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local noundef float @_Z1tf(float noundef %0) #0 !dbg !10 {
  %2 = alloca float, align 4
  store float %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !16, metadata !DIExpression()), !dbg !17
  %3 = load float, ptr %2, align 4, !dbg !18
  %4 = fdiv float %3, 2.000000e+00, !dbg !19
  ret float %4, !dbg !20
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local noundef i32 @_Z6squarei(i32 noundef %0) #0 !dbg !21 {
  %2 = alloca i32, align 4
  %3 = alloca float, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !25, metadata !DIExpression()), !dbg !26
  call void @llvm.dbg.declare(metadata ptr %3, metadata !27, metadata !DIExpression()), !dbg !28
  %4 = load i32, ptr %2, align 4, !dbg !29
  %5 = sitofp i32 %4 to float, !dbg !29
  %6 = call noundef float @_Z1tf(float noundef %5), !dbg !30
  store float %6, ptr %3, align 4, !dbg !28
  %7 = load i32, ptr %2, align 4, !dbg !31
  %8 = load i32, ptr %2, align 4, !dbg !32
  %9 = mul nsw i32 %7, %8, !dbg !33
  ret i32 %9, !dbg !34
}

attributes #0 = { mustprogress noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5, !6, !7, !8}
!llvm.ident = !{!9}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "clang version 15.0.0 (https://github.com/llvm/llvm-project.git 4ba6a9c9f65bbc8bd06e3652cb20fd4dfc846137)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/app/example.cpp", directory: "/app")
!2 = !{i32 7, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !{i32 1, !"wchar_size", i32 4}
!5 = !{i32 7, !"PIC Level", i32 2}
!6 = !{i32 7, !"PIE Level", i32 2}
!7 = !{i32 7, !"uwtable", i32 2}
!8 = !{i32 7, !"frame-pointer", i32 2}
!9 = !{!"clang version 15.0.0 (https://github.com/llvm/llvm-project.git 4ba6a9c9f65bbc8bd06e3652cb20fd4dfc846137)"}
!10 = distinct !DISubprogram(name: "t", linkageName: "_Z1tf", scope: !11, file: !11, line: 2, type: !12, scopeLine: 2, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !15)
!11 = !DIFile(filename: "example.cpp", directory: "/app")
!12 = !DISubroutineType(types: !13)
!13 = !{!14, !14}
!14 = !DIBasicType(name: "float", size: 32, encoding: DW_ATE_float)
!15 = !{}
!16 = !DILocalVariable(name: "b", arg: 1, scope: !10, file: !11, line: 2, type: !14)
!17 = !DILocation(line: 2, column: 15, scope: !10)
!18 = !DILocation(line: 3, column: 12, scope: !10)
!19 = !DILocation(line: 3, column: 14, scope: !10)
!20 = !DILocation(line: 3, column: 5, scope: !10)
!21 = distinct !DISubprogram(name: "square", linkageName: "_Z6squarei", scope: !11, file: !11, line: 6, type: !22, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !15)
!22 = !DISubroutineType(types: !23)
!23 = !{!24, !24}
!24 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!25 = !DILocalVariable(name: "num", arg: 1, scope: !21, file: !11, line: 6, type: !24)
!26 = !DILocation(line: 6, column: 16, scope: !21)
!27 = !DILocalVariable(name: "x", scope: !21, file: !11, line: 7, type: !14)
!28 = !DILocation(line: 7, column: 11, scope: !21)
!29 = !DILocation(line: 7, column: 17, scope: !21)
!30 = !DILocation(line: 7, column: 15, scope: !21)
!31 = !DILocation(line: 8, column: 12, scope: !21)
!32 = !DILocation(line: 8, column: 18, scope: !21)
!33 = !DILocation(line: 8, column: 16, scope: !21)
!34 = !DILocation(line: 8, column: 5, scope: !21)
