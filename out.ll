; ModuleID = 'main'
source_filename = "main"

declare void @ll_putint(i32)

declare void @ll_print_line()

define i32 @lettest(i32 %a, i32 %b) {
entry:
  call void @ll_putint(i32 30)
  call void @ll_print_line()
  %call_tmp = call i32 @inner_adder(i32 10, i32 10)
  ret i32 %call_tmp
}

define i32 @inner_adder(i32 %a, i32 %b) {
entry:
  %add_tmp = add i32 %a, %b
  ret i32 %add_tmp
}

define i32 @sub(i32 %a, i32 %b) {
entry:
  %sub_tmp = sub i32 %a, %b
  call void @ll_putint(i32 %sub_tmp)
  call void @ll_print_line()
  %sub_tmp1 = sub i32 %a, %b
  ret i32 %sub_tmp1
}

define void @add(i32 %a, i32 %b) {
entry:
  %add_tmp = add i32 %a, %b
  call void @ll_putint(i32 %add_tmp)
  call void @ll_print_line()
  ret void
}

define i32 @adder(i32 %a, i32 %b) {
entry:
  %add_tmp = add i32 %a, %b
  %eq_cmp = icmp eq i32 %add_tmp, 10
  br i1 %eq_cmp, label %then, label %else

then:                                             ; preds = %entry
  %add_tmp1 = add i32 %a, %b
  br label %if_cont

else:                                             ; preds = %entry
  br label %if_cont

if_cont:                                          ; preds = %else, %then
  %if_result = phi i32 [ %add_tmp1, %then ], [ 50, %else ]
  ret i32 %if_result
}

define i32 @id(i32 %x) {
entry:
  ret i32 %x
}

define i32 @fn2(i32 %a, i32 %b) {
entry:
  %call_tmp = call i32 @adder2(i32 %a, i32 %b)
  ret i32 %call_tmp
}

define i32 @adder2(i32 %x, i32 %y) {
entry:
  %add_tmp = add i32 %x, %y
  ret i32 %add_tmp
}

define i32 @main() {
entry:
  call void @ll_print_line()
  %call_tmp = call i32 @lettest(i32 100, i32 99)
  call void @ll_putint(i32 %call_tmp)
  call void @ll_print_line()
  %call_tmp1 = call i32 @fn2(i32 1, i32 2)
  call void @ll_putint(i32 %call_tmp1)
  call void @ll_print_line()
  %call_tmp2 = call i32 @adder(i32 5, i32 5)
  call void @ll_putint(i32 %call_tmp2)
  call void @ll_print_line()
  call void @res(i32 5)
  call void @ll_print_line()
  ret i32 0
}

define void @res(i32 %a) {
entry:
  %call_tmp = call i32 @id(i32 3)
  %eq_cmp = icmp eq i32 4, %call_tmp
  br i1 %eq_cmp, label %then, label %else

then:                                             ; preds = %entry
  call void @ll_putint(i32 1)
  br label %if_cont9

else:                                             ; preds = %entry
  %call_tmp1 = call i32 @id(i32 6)
  %eq_cmp2 = icmp eq i32 6, %call_tmp1
  br i1 %eq_cmp2, label %then3, label %else4

then3:                                            ; preds = %else
  call void @ll_putint(i32 2)
  br label %if_cont8

else4:                                            ; preds = %else
  %call_tmp5 = call i32 @id(i32 4)
  %eq_cmp6 = icmp eq i32 4, %call_tmp5
  br i1 %eq_cmp6, label %then7, label %if_cont

then7:                                            ; preds = %else4
  call void @ll_putint(i32 3)
  br label %if_cont

if_cont:                                          ; preds = %then7, %else4
  br label %if_cont8

if_cont8:                                         ; preds = %if_cont, %then3
  br label %if_cont9

if_cont9:                                         ; preds = %if_cont8, %then
  ret void
}

