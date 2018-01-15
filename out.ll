declare void @ll_putint(i32)
declare void @ll_putchar(i8)

define i32 @adder(i32 %a, i32 %b) {
Entry:
  %addtmp = add i32 %a, %b
  %multmp = mul i32 %addtmp, 8
  %addtmp1 = add i32 %a, %b
  %addtmp2 = add i32 %a, %b
  %divtmp = sdiv i32 %addtmp2, 2
  ret i32 6
}

define i32 @main() {
  %tmp1 = call i32 @adder (i32 1, i32 2)
  call void @ll_putint(i32 %tmp1)
  call void @ll_putchar(i8 10)
  ret i32 0
}
