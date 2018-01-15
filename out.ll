declare void @ll_putint(i32)
declare void @ll_putchar(i8)

define i32 @adder(i32 %a, i32 %b) {
Entry:
  %addtmp = add i32 %a, %b
  %eqcmp = icmp eq i32 %addtmp, 3
  br i1 %eqcmp, label %then, label %else

then:                                             ; preds = %Entry
  br label %ifcont

else:                                             ; preds = %Entry
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ 100, %then ], [ 50, %else ]
  ret i32 %iftmp
}


define i32 @main() {
  %tmp1 = call i32 @adder (i32 1, i32 4)
  call void @ll_putint(i32 %tmp1)
  call void @ll_putchar(i8 10)
  ret i32 0
}
