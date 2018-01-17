

declare void @ll_putint(i32)


declare void @ll_print_line()


define i32 @sub(i32 %a, i32 %b) {
Entry:
  %sub_tmp = sub i32 %a, %b
  call void @ll_putint(i32 %sub_tmp)
  call void @ll_print_line()
  %sub_tmp1 = sub i32 %a, %b
  ret i32 %sub_tmp1
}


define void @add(i32 %a, i32 %b) {
Entry:
  %add_tmp = add i32 %a, %b
  call void @ll_putint(i32 %add_tmp)
  call void @ll_print_line()
  ret void
}


define i32 @adder(i32 %a, i32 %b) {
Entry:
  %add_tmp = add i32 %a, %b
  %eq_cmp = icmp eq i32 %add_tmp, -121
  br i1 %eq_cmp, label %then, label %else

then:                                             ; preds = %Entry
  br label %if_cont

else:                                             ; preds = %Entry
  br label %if_cont

if_cont:                                          ; preds = %else, %then
  %if_tmp = phi i32 [ 868659472, %then ], [ -6377551, %else ]
  ret i32 %if_tmp
}


define i32 @id(i32 %x) {
Entry:
  ret i32 %x
}


define i32 @main() {
Entry:
  %call_tmp = call i32 @adder(i32 -5, i32 -31)
  call void @ll_putint(i32 %call_tmp)
  call void @ll_print_line()
  call void @add(i32 -168421, i32 -168452)
  %call_tmp1 = call i32 @id(i32 -781)
  %add_tmp = add i32 %call_tmp1, i32 -781
  call void @ll_putint(i32 %add_tmp)
  %call_tmp2 = call i32 @sub(i32 -168421, i32 -168421)
  %call_tmp3 = call i32 @sub(i32 -168421, i32 -168421)
  %add_tmp4 = add i32 %call_tmp2, %call_tmp3
  call void @ll_putint(i32 %add_tmp4)
  ret i32 -1
}

