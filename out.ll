

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
  %eq_cmp = icmp eq i32 %add_tmp, 3
  br i1 %eq_cmp, label %then, label %else

then:                                             ; preds = %Entry
  br label %if_cont

else:                                             ; preds = %Entry
  br label %if_cont

if_cont:                                          ; preds = %else, %then
  %if_tmp = phi i32 [ 100, %then ], [ 50, %else ]
  ret i32 %if_tmp
}


define i32 @id(i32 %x) {
Entry:
  ret i32 %x
}


define i32 @main() {
Entry:
  %call_tmp = call i32 @adder(i32 1, i32 2)
  call void @ll_putint(i32 %call_tmp)
  call void @ll_print_line()
  call void @add(i32 20, i32 22)
  %call_tmp1 = call i32 @sub(i32 40, i32 20)
  %call_tmp2 = call i32 @sub(i32 21, i32 20)
  %add_tmp = add i32 %call_tmp1, %call_tmp2
  call void @ll_putint(i32 %add_tmp)
  ret i32 0
}

