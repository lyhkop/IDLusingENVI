Pro get_regression_analysis

  compile_opt idl2

  envi, /restore_base_save_files
  envi_batch_init
  
  input_filename = dialog_pickfile(title='请选择输入数据')
  if (input_filename eq '') then return
  
  output_filename = dialog_pickfile(title='请选择输出数据', /write)
  if (output_filename eq '') then return
  
  ;获取输入文件信息，若不为单波段则返回
  envi_open_file, input_filename, r_fid=input_fileid
  if (input_fileid eq -1) then return
  envi_file_query, input_fileid, ns=ns, nl=nl, nb=nb, dims=dims, $
                   data_type=data_type
  if (nb ne 1) then begin
    dialog_result = dialog_message(title='警告信息', ['输入数据为多波段', '模型将应用于第一个波段！'], /information)
  endif
  
  ;构建输入参数的界面
  wbase = widget_auto_base(title='回归模型参数设置')
    
    model_list = ['线性模型:Y=0.81*b1+0.143', '二次项模型:Y=-0.43*b1^2+1.28*b1+0.05', $
                  '三次项模型:Y=-2.24*b1^3+3.29*b1^2-0.45*b1+0.23', '幂函数模型:Y=0.925*b1^0.694', $
                  '指数模型:Y=e^(b1-1.070)', 'Logistic模型:Y=0.99+(0.21-0.99)/(1+(b1/0.53)^2.84)', $
                  '对数模型:Y=alog(b1+1.294)']
    wslist = widget_slist(wbase, list=model_list, prompt='回归模型列表', select_prompt='选择的模型', uvalue='wslist', default=0, /auto)
    wstring = widget_string(wbase, uvalue='wstring', prompt='输入模型表达式', $ 
                            default='0.81*b1+0.143', /auto)
  wresult = auto_wid_mng(wbase)  
  if (wresult.accept eq 0) then return
  exp = wresult.wstring
  
  envi_doit, 'math_doit', fid=input_fileid, pos=1, $
  dims=dims, out_bname='huigui', out_name=output_filename, $
  exp=exp 
  
  envi_batch_exit

End