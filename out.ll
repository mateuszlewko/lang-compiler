; ModuleID = 'interactive'
source_filename = "interactive"

@arr_val = global [0 x i32]* null
@topval6_val = global i32 0
@topval7_val = global i32 0
@topval_val = global i32 0
@retfun_val = global i32 (i32)* null
@my_arr_val = global [0 x i32]* null

declare void @ll_putint(i32)

declare void @ll_print_line()

declare i32 @ll_get_ith_elem_of_int_array([0 x i32]*, i32)

declare void @ll_set_ith_elem_of_int_array([0 x i32]*, i32, i32)

declare [0 x i32]* @ll_new_int_array(i32)

define void @A.aaaaaa() {
entry:
  tail call void @ll_putint(i32 1)
  tail call void @ll_print_line()
  ret void
}

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
  %call_tmp = tail call i32 @D.E.adder(i32 21, i32 %D.x)
  ret i32 %call_tmp
}

define i32 @apply(i32 (i32)* %fn, i32 %arg) {
entry:
  %call_tmp = tail call i32 %fn(i32 %arg)
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
  %call_tmp = tail call i32 @power(i32 %a, i32 %sub_tmp)
  %mul_tmp = mul i32 %a, %call_tmp
  br label %if_cont

if_cont:                                          ; preds = %else, %then
  %if_result = phi i32 [ 1, %then ], [ %mul_tmp, %else ]
  ret i32 %if_result
}

define i32 @fib(i32 %n) {
entry:
  %call_tmp = tail call i32 @aux(i32 %n, i32 0, i32 1)
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
  %call_tmp = tail call i32 @aux(i32 %sub_tmp, i32 %b, i32 %add_tmp)
  br label %if_cont

if_cont:                                          ; preds = %else, %then
  %if_result = phi i32 [ %b, %then ], [ %call_tmp, %else ]
  ret i32 %if_result
}

define i32 @app2(i32 (i32, i32)* %fn, i32 %arg1, i32 %arg2) {
entry:
  %call_tmp = tail call i32 %fn(i32 %arg1, i32 %arg2)
  ret i32 %call_tmp
}

define i32 @G.testG(i32 %G.a, i32 %G.b) {
entry:
  %add_tmp = add i32 %G.a, %G.b
  ret i32 %add_tmp
}

define [0 x i32]* @arr() {
entry:
  %malloccall = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i64), i64 3) to i32))
  %malloc_tmp = bitcast i8* %malloccall to [3 x i32]*
  store [3 x i32] [i32 3, i32 5, i32 10111], [3 x i32]* %malloc_tmp
  %exp_ptr32 = bitcast [3 x i32]* %malloc_tmp to [0 x i32]*
  ret [0 x i32]* %exp_ptr32
}

declare noalias i8* @malloc(i32)

define i32 @topval6() {
entry:
  ret i32 77
}

define i32 @topval7() {
entry:
  %load_res = load i32, i32* @topval6_val
  %add_tmp = add i32 %load_res, 3
  ret i32 %add_tmp
}

define i32 @topval8(i32 %x) {
entry:
  %load_res = load i32, i32* @topval7_val
  %add_tmp = add i32 %load_res, 7
  %add_tmp1 = add i32 %add_tmp, %x
  ret i32 %add_tmp1
}

define i32 @topval() {
entry:
  ret i32 4
}

define i32 (i32)* @retfun() {
entry:
  ret i32 (i32)* @mult2
}

define i32 @get_ith([0 x i32]* %arr, i32 %ix) {
entry:
  %call_tmp = tail call i32 @ll_get_ith_elem_of_int_array([0 x i32]* %arr, i32 %ix)
  ret i32 %call_tmp
}

define void @set_ith([0 x i32]* %arr, i32 %ix, i32 %val) {
entry:
  tail call void @ll_set_ith_elem_of_int_array([0 x i32]* %arr, i32 %ix, i32 %val)
  ret void
}

define [0 x i32]* @new(i32 %size) {
entry:
  %call_tmp = tail call [0 x i32]* @ll_new_int_array(i32 %size)
  ret [0 x i32]* %call_tmp
}

define [0 x i32]* @my_arr() {
entry:
  %call_tmp = tail call [0 x i32]* @new(i32 100)
  ret [0 x i32]* %call_tmp
}

define i32 @add_one(i32 %x, i32 %n) {
entry:
  %eq_cmp = icmp eq i32 %n, 0
  br i1 %eq_cmp, label %then, label %else

then:                                             ; preds = %entry
  br label %if_cont

else:                                             ; preds = %entry
  %add_tmp = add i32 1, %x
  %sub_tmp = sub i32 %n, 1
  %call_tmp = tail call i32 @add_one(i32 %add_tmp, i32 %sub_tmp)
  br label %if_cont

if_cont:                                          ; preds = %else, %then
  %if_result = phi i32 [ %x, %then ], [ %call_tmp, %else ]
  ret i32 %if_result
}

define i32 @main() {
calls_to_top_vals:
  call void @A.aaaaaa()
  %ret = call [0 x i32]* @arr()
  store [0 x i32]* %ret, [0 x i32]** @arr_val
  %ret21 = call i32 @topval6()
  store i32 %ret21, i32* @topval6_val
  %ret22 = call i32 @topval7()
  store i32 %ret22, i32* @topval7_val
  %ret23 = call i32 @topval()
  store i32 %ret23, i32* @topval_val
  %ret24 = call i32 (i32)* @retfun()
  store i32 (i32)* %ret24, i32 (i32)** @retfun_val
  %ret25 = call [0 x i32]* @my_arr()
  store [0 x i32]* %ret25, [0 x i32]** @my_arr_val
  br label %entry

entry:                                            ; preds = %calls_to_top_vals
  %call_tmp = tail call i32 @add_one(i32 0, i32 100000000)
  tail call void @ll_putint(i32 %call_tmp)
  tail call void @ll_print_line()
  %load_res = load [0 x i32]*, [0 x i32]** @my_arr_val
  tail call void @set_ith([0 x i32]* %load_res, i32 78, i32 108)
  %load_res1 = load [0 x i32]*, [0 x i32]** @my_arr_val
  %call_tmp2 = tail call i32 @get_ith([0 x i32]* %load_res1, i32 78)
  tail call void @ll_putint(i32 %call_tmp2)
  tail call void @ll_print_line()
  %load_res3 = load [0 x i32]*, [0 x i32]** @my_arr_val
  %call_tmp4 = tail call i32 @get_ith([0 x i32]* %load_res3, i32 79)
  tail call void @ll_putint(i32 %call_tmp4)
  tail call void @ll_print_line()
  %load_res5 = load [0 x i32]*, [0 x i32]** @arr_val
  %call_tmp6 = tail call i32 @get_ith([0 x i32]* %load_res5, i32 2)
  tail call void @ll_putint(i32 %call_tmp6)
  tail call void @ll_print_line()
  %load_res7 = load [0 x i32]*, [0 x i32]** @arr_val
  tail call void @set_ith([0 x i32]* %load_res7, i32 2, i32 999)
  %load_res8 = load [0 x i32]*, [0 x i32]** @arr_val
  %call_tmp9 = tail call i32 @get_ith([0 x i32]* %load_res8, i32 2)
  tail call void @ll_putint(i32 %call_tmp9)
  tail call void @ll_print_line()
  %load_res10 = load i32 (i32)*, i32 (i32)** @retfun_val
  %call_tmp11 = tail call i32 %load_res10(i32 109)
  tail call void @ll_putint(i32 %call_tmp11)
  tail call void @ll_print_line()
  %load_res12 = load i32, i32* @topval7_val
  tail call void @ll_putint(i32 %load_res12)
  tail call void @ll_print_line()
  %call_tmp13 = tail call i32 @topval8(i32 7)
  tail call void @ll_putint(i32 %call_tmp13)
  tail call void @ll_print_line()
  %call_tmp14 = tail call i32 @G.testG(i32 2, i32 3)
  tail call void @ll_putint(i32 %call_tmp14)
  tail call void @ll_print_line()
  %call_tmp15 = tail call i32 @A.testA(i32 101)
  tail call void @ll_putint(i32 %call_tmp15)
  tail call void @ll_print_line()
  %call_tmp16 = tail call i32 @D.testD(i32 35)
  tail call void @ll_putint(i32 %call_tmp16)
  tail call void @ll_print_line()
  %call_tmp17 = tail call i32 @fib(i32 35)
  tail call void @ll_putint(i32 %call_tmp17)
  tail call void @ll_print_line()
  %call_tmp18 = tail call i32 @apply(i32 (i32)* @mult2, i32 6)
  tail call void @ll_putint(i32 %call_tmp18)
  tail call void @ll_print_line()
  %call_tmp19 = tail call i32 @power(i32 3, i32 4)
  tail call void @ll_putint(i32 %call_tmp19)
  tail call void @ll_print_line()
  %call_tmp20 = tail call i32 @app2(i32 (i32, i32)* @power, i32 2, i32 10)
  tail call void @ll_putint(i32 %call_tmp20)
  tail call void @ll_print_line()
  tail call void @ll_putint(i32 104)
  tail call void @ll_print_line()
  ret i32 0
}
 