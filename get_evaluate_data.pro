;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释[~_~];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; 函数名：
; MyShift
; 调用方式:
; MyShift(data, d)
; 
; 目的:
; 平移数组中的元素，平移后的边界值用0补齐
; 如：A=[1, 2, 3, 4, 5]向右平移两个元素，则返回[0, 0, 1, 2, 3]
;
; 输入参数:
; data 进行平移的数组
; d    平移距离，d为正值将向左平移反之向右平移
; 
; 函数返回值:
; r_data 平移后的数组
;
; 备注：
;+
;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释[~_~];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Function MyShift, data, d
;
  if d eq 0 then begin
    return, data
  endif

;d大于0向左平移，d小于0向右平移
  if d gt 0 then begin
    for i=0l, n_elements(data)-1 do begin
      if i lt n_elements(data)-d then begin
        data[i] = data[i+d]
      endif else begin
        data[i] = 0
      endelse
    endfor
  endif else begin
    d=abs(d)
    for i=0l, n_elements(data)-1 do begin
      if i lt n_elements(data)-d then begin
        data[n_elements(data)-1-i] = data[n_elements(data)-1-i-d]
      endif else begin
        data[n_elements(data)-1-i] = 0
      endelse
    endfor
  endelse
  return, data
End



;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释[~_~];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 函数名：
; PingJia(data1, data2, flag1, flag2)
;
; 目的：
; 数组data2与数组data1进行差值或比值运算即：data2-data1
;
; 输入参数：
; flag1:数组匹配方式(0：最大值匹配、1：最小值匹配)
; flag2:评价方式，(0:差值评价  、    1：比值评价)
; 返回值：
; 返回 data2-data1  or  data2/data1
;
; 备注：
;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释\~_~\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Function PingJia, data1, data2, flag1, flag2
  ;
  COMPILE_OPT idl2
  ;检测参数是否正确
  ;dims = SIZE(data1)
  ;确定最大值和最小值位置
  max_value1 = max(data1, max_subscript1, SUBSCRIPT_MIN=min_subscript1)
  max_value2 = max(data2, max_subscript2, SUBSCRIPT_MIN=min_subscript2)
  
  if flag1 eq 0 then begin
    move_distance = max_subscript2 - max_subscript1
  endif else begin
    move_distance = min_subscript2 - min_subscript1
  endelse 
  
  ;匹配并平移data2
  data2 = myshift(data2, move_distance)
  
  outdata = make_array(n_elements(data1))
  ;去掉头尾只评价共有的部分数据,头尾评价值为100
  ;data2与data1差值或比值运算
  if (flag2 eq 0) then begin 
    ;outdata = data2 - data1
    for i=0, n_elements(data1)-1 do begin
      if data2[i] eq 0 then begin
        outdata[i] = -100
      endif else begin
        outdata[i] = data2[i] - data1[i]
      endelse
    endfor
          
  endif else begin
    ;比值比较
    for i=0, n_elements(data1)-1 do begin
      if (data2[i] eq 0) or (data1[i] eq 0) then begin
        outdata[i] = -100
      endif else begin
        outdata[i] = data2[i] / data1[i] 
      endelse      
    endfor
    
  endelse
  
  return, outdata
  
End


Pro get_evaluate_data

  compile_opt idl2
  
  envi, /restore_base_save_files
  envi_batch_init
  
  input_filename_1 = ENVI_PICKFILE(TITLE='请输入多年平均数据')
  if n_elements(input_filename_1) eq 0 then return  
  
  input_filename_2 = envi_pickfile(title='请输入待评价数据')
  if n_elements(input_filename_2) eq 0 then return
  
  output_filename = envi_pickfile(title='输出文件名' ,/output)
  if (output_filename eq '') then return
  
  wbase = widget_auto_base(title='选择曲线匹配方式')
    wtoggle_1 = widget_toggle(wbase, list=['最大值匹配', '最小值匹配'], prompt='选择匹配方式:', $
    uvalue='mtoggle_1', /auto)
    
    wtoggle_2 = widget_toggle(wbase, list=['差值评价', '比值评价'], prompt='选择匹配方式:', $
    uvalue='mtoggle_2', /auto)
    
  mresult = auto_wid_mng(wbase)
  if (mresult.accept eq 0) then return  
  pipei_flag = mresult.mtoggle_1
  pingjia_flag = mresult.mtoggle_2
  
  envi_open_file, input_filename_1, r_fid=input_fileid_1
  envi_open_file, input_filename_2, r_fid=input_fileid_2
  if ((input_fileid_1 eq -1) or (input_fileid_2 eq -1)) then return
  
  envi_file_query, input_fileid_1, dims=dims, $
  ns=ns, $
  nl=nl, $
  nb=nb, $
  data_type=data_type, $
  interleave=interleave
  
  ;打开输出文件
  openw, output_file_unit, output_filename, /get_lun
  
  ;分块处理数据
  tile_nums = 10
  tile_lines = [nl/tile_nums, nl/tile_nums, $
                nl/tile_nums, nl/tile_nums, $
                nl/tile_nums, nl/tile_nums, $
                nl/tile_nums, nl/tile_nums, $
                nl/tile_nums, nl-nl/tile_nums*9]
                
  ;循环处理Tiles的内容
  for i=0l, tile_nums-1 do begin
    ;平均数据
    input_data_1 = make_array(ns, tile_lines[i], $
    nb)
    ;待评价数据
    input_data_2 = make_array(ns, tile_lines[i], $
    nb)
    ;output_data输出评价数据
    output_data = make_array(ns, tile_lines[i], $
    nb)
    for j=0l, nb-1 do begin
      input_data_1[*, *, j] = envi_get_data(fid=input_fileid_1, $
      dims = [dims[0], $
              dims[1], $
              dims[2], $
              i*nl/tile_nums, $
              i*nl/tile_nums + tile_lines[i]-1], $
      pos = j )
      input_data_2[*, *, j] = envi_get_data(fid=input_fileid_2, $
      dims = [dims[0], $
              dims[1], $
              dims[2], $
              i*nl/tile_nums, $
              i*nl/tile_nums + tile_lines[i]-1], $
      pos = j )      
    endfor

    ;对数据进行评价, 平移平均数据, 带评价数据固定
    for m=0l, ns-1 do begin
      for n=0l, tile_lines[i]-1 do begin
        output_data[m, n, *] = pingjia(input_data_2[m, n, *], input_data_1[m, n, *], $
                                     pipei_flag, pingjia_flag)
      endfor
    endfor
  
    ;将数据存储方式转换为BIL，并写入输出文件中
    output_data = transpose(temporary(output_data), [0, 2, 1])
    writeu, output_file_unit, output_data
    
    print, '第', i, '块'
  
  endfor
  
  ;关闭文件
  free_lun, output_file_unit
  
  ;data_type:float
  map_info = envi_get_map_info(fid=input_fileid_1)
  envi_setup_head, fname=output_filename, ns=ns, nl=nl, nb=nb, map_info=map_info, $ 
  data_type=4, offset=0, interleave=1, $
  descrip='Test routine output', /write, /open
    
  envi_batch_exit

End