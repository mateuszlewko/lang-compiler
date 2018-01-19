; ModuleID = 'main'
source_filename = "main"

; declare void @ll_putint(i32)

; declare void @ll_print_line()

@.str = private unnamed_addr constant [3 x i8] c"%d\00", align 1

define i32 @apply(i32 (i32)* %fn, i32 %arg) {
entry:
  %call_tmp = call i32 %fn(i32 %arg)
  ret i32 %call_tmp
}

define i32 @mult2(i32 %x) {
entry:
  %mul_tmp = mul i32 %x, 2
  ret i32 %mul_tmp
}

define i32 @main() {
entry:
  %call_tmp = call i32 @apply(i32 (i32)* @mult2, i32 6)
  ; call void @ll_putint(i32 %call_tmp)
  ; call void @ll_print_line()
  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i32 0, i32 0), i32 %call_tmp)

  ret i32 0
}


declare i32 @printf(i8*, ...) #1

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 5.0.1 (tags/RELEASE_501/final)"}
