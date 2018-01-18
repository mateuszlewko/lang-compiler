; ModuleID = 'main'
source_filename = "main"

declare void @ll_putint(i32)

declare void @ll_print_line()

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

define i32 @main() {
entry:
  %call_tmp = call i32 @power(i32 3, i32 4)
  call void @ll_putint(i32 %call_tmp)
  call void @ll_print_line()
  ret i32 0
}

