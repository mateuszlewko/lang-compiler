; ModuleID = 'interactive'
source_filename = "interactive"

declare void @ll_putint(i32)

declare void @ll_print_line()

define i32 @A.testA(i32 %A.x) {
entry:
  %mul_tmp = mul i32 %A.x, 2
  ret i32 %mul_tmp
}

define i32 @A.B.testB(i32 %A.B.x, i32 %A.B.y) {
entry:
  %add_tmp = add i32 %A.B.x, %A.B.y
  ret i32 %add_tmp
}

define i32 @D.E.adder(i32 %D.E.x, i32 %D.E.y) {
entry:
  %add_tmp = add i32 %D.E.x, %D.E.y
  ret i32 %add_tmp
}

define i32 @D.testD(i32 %D.x) {
entry:
  %call_tmp = call i32 @D.E.adder(i32 21, i32 %D.x)
  ret i32 %call_tmp
}

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

define i32 @power(i32 %a, i32 %n) {
entry:
  %eq_cmp = icmp eq i32 %n, 0
  br i1 %eq_cmp, label %then, label %else

then:                                             ; preds = %entry
  br label %if_cont

else:                                             ; preds = %entry
  %sub_tmp = sub i32 %n, 1
  %call_tmp = call i32 @power(i32 %a, i32 %sub_tmp)
  %mul_tmp = mul i32 %a, %call_tmp
  br label %if_cont

if_cont:                                          ; preds = %else, %then
  %if_result = phi i32 [ 1, %then ], [ %mul_tmp, %else ]
  ret i32 %if_result
}

define i32 @fib(i32 %n) {
entry:
  %call_tmp = call i32 @aux(i32 %n, i32 0, i32 1)
  ret i32 %call_tmp
}

define i32 @aux(i32 %n, i32 %a, i32 %b) {
entry:
  %eq_cmp = icmp eq i32 %n, 0
  br i1 %eq_cmp, label %then, label %else

then:                                             ; preds = %entry
  br label %if_cont

else:                                             ; preds = %entry
  %sub_tmp = sub i32 %n, 1
  %add_tmp = add i32 %a, %b
  %call_tmp = call i32 @aux(i32 %sub_tmp, i32 %b, i32 %add_tmp)
  br label %if_cont

if_cont:                                          ; preds = %else, %then
  %if_result = phi i32 [ %b, %then ], [ %call_tmp, %else ]
  ret i32 %if_result
}

define i32 @app2(i32 (i32, i32)* %fn, i32 %arg1, i32 %arg2) {
entry:
  %call_tmp = call i32 %fn(i32 %arg1, i32 %arg2)
  ret i32 %call_tmp
}

define i32 @G.testG(i32 %G.a, i32 %G.b) {
entry:
  %add_tmp = add i32 %G.a, %G.b
  ret i32 %add_tmp
}

define i32 @main() {
entry:
  %call_tmp = call i32 @G.testG(i32 2, i32 3)
  call void @ll_putint(i32 %call_tmp)
  call void @ll_print_line()
  %call_tmp1 = call i32 @A.testA(i32 101)
  call void @ll_putint(i32 %call_tmp1)
  call void @ll_print_line()
  %call_tmp2 = call i32 @D.testD(i32 35)
  call void @ll_putint(i32 %call_tmp2)
  call void @ll_print_line()
  %call_tmp3 = call i32 @fib(i32 35)
  call void @ll_putint(i32 %call_tmp3)
  call void @ll_print_line()
  %call_tmp4 = call i32 @apply(i32 (i32)* @mult2, i32 6)
  call void @ll_putint(i32 %call_tmp4)
  call void @ll_print_line()
  %call_tmp5 = call i32 @power(i32 3, i32 4)
  call void @ll_putint(i32 %call_tmp5)
  call void @ll_print_line()
  %call_tmp6 = call i32 @app2(i32 (i32, i32)* @power, i32 2, i32 10)
  call void @ll_putint(i32 %call_tmp6)
  call void @ll_print_line()
  call void @ll_putint(i32 104)
  ret i32 0
}

