declare void @ll_putint(i32)
declare void @ll_putchar(i8)

define i32 @myfun(i32 %a, i32 %b) {
Entry:
  %addtmp = add i32 5, %b
  %addtmp1 = add i32 %a, %b
  %addtmp2 = add i32 %addtmp, %addtmp1
  ret i32 %addtmp2
}

define i32 @main() {
  %tmp1 = call i32 @myfun(i32 1, i32 2)
  call void @ll_putint(i32 %tmp1)
  call void @ll_putchar(i8 10)
  ret i32 0
}
