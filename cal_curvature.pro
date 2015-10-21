Pro logistic_fit, X, Y

  compile_opt idl2
  
  

End

;计算离散点曲线的曲率
;Y=X(i), X0=0, X1=1, X2=2, ......
;该算法默认离散点X坐标的间隔为1
Pro cal_curvature, Y, curvature

  compile_opt idl2
  
  ;size()返回值为[维度, 类型, 各维度元素个数, 元素总个数]
  TypeX = size(Y)
  if (TypeX[0] ne 1) then begin
    message, '请输入一维数组'
  endif
  
  ;获得输入数据的一和二阶阶导数
  dy_1 = deriv(Y)
  dy_2 = deriv(dy_1)
  
  ;获得离散点的曲率
  curvature = dy_2 / (1 + dy_1^2)^(3/2)
  
End


Pro get_max_curvature_position

  compile_opt idl2
  
  envi, /restore_base_save_files
  envi_batch_init
  
  input_file = dialog_pickfile(title='请选择输入文件')
  if (input_file eq '') then return
  
  output_file = dialog_pickfile(title='选择输出文件', /write)
  if (output_file eq '') then return
  
  envi_open_file, input_file, r_fid=fid
  if (fid eq -1) then begin
    return_dialog = dialog_message(['打开文件失败: ', '该文件无法使用envi_open_file函数'], title='错误信息', /information)
    return
  endif
  envi_file_query, fid, dims=dims, ns=ns, nl=nl, nb=nb, data_type=data_type
  
 ;大于300M则进行分块处理
  file_size = ns*nl*nb*data_type_to_byte(data_type)
  max_file_size = 300*1024*1024
  
  if (file_size ge max_file_size) then begin
    
    print, '分块处理'

    
  endif else begin
    
    print, '整体处理'

  
  endelse
  
  dialog_result = dialog_message(title='提示信息', ['数据已生成'], /information)
  
  ;envi_batch_exit  
  
End