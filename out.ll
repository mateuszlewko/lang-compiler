
decl func lettest
 LL: ; ModuleID = 'LetModule'
source_filename = "LetModule"

declare i32 @lettest(i32, i32)
decl func inner_adder
 LL: ; ModuleID = 'LetModule'
source_filename = "LetModule"

declare i32 @inner_adder(i32, i32)
decl func sub
 LL: ; ModuleID = 'LetModule'
source_filename = "LetModule"

declare i32 @sub(i32, i32)
decl func add
 LL: ; ModuleID = 'LetModule'
source_filename = "LetModule"

declare void @add(i32, i32)
decl func adder
 LL: ; ModuleID = 'LetModule'
source_filename = "LetModule"

declare i32 @adder(i32, i32)
decl func id
 LL: ; ModuleID = 'LetModule'
source_filename = "LetModule"

declare i32 @id(i32)
decl func fn2
 LL: ; ModuleID = 'LetModule'
source_filename = "LetModule"

declare i32 @fn2(i32, i32)
decl func adder
 LL: ; ModuleID = 'LetModule'
source_filename = "LetModule"

declare i32 @adder(i32, i32)
decl func main
 LL: ; ModuleID = 'LetModule'
source_filename = "LetModule"

declare i32 @main()

declare void @ll_putint(i32)


declare void @ll_print_line()


define i32 @lettest(i32 %a, i32 %b) {
Entry:
  call void @ll_putint(i32 30)
  %call_tmp = call i32 @inner_adder(i32 10, i32 10)
  ret i32 %call_tmp
}


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


define i32 @fn2(i32 %a, i32 %b) {
Entry:
  %call_tmp = call i32 @adder(i32 %a, i32 %b)
  ret i32 %call_tmp
}


define i32 @main() {
Entry:
  call void @ll_print_line()
  %call_tmp = call i32 @fn2(i32 1, i32 2)
  call void @ll_putint(i32 %call_tmp)
  call void @ll_print_line()
  ret i32 0
}

