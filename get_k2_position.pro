;根据数据类型返回占用字节数
;1: Byte (8 bits) 
;2: Integer (16 bits) 
;3: Long integer (32 bits) 
;4: Floating-point (32 bits) 
;5: Double-precision floating-point (64 bits) 
;6: Complex (2x32 bits) 
;9: Double-precision complex (2x64 bits) 
;12: Unsigned integer (16 bits) 
;13: Unsigned long integer (32 bits) 
;14: Long 64-bit integer 
;15: Unsigned long 64-bit integer
Function data_type_to_byte, data_type
  
  compile_opt idl2
  
  switch (data_type) of
    1: begin
      return, 1
      break
    end
    2: begin
      return, 2
      break
    end
    3: begin
      return, 4
      break
    end
    4: begin
      return, 4
      break
    end
    5: begin
      return, 8
      break
    end
    6: begin
      return, 8
      break
    end
    9: begin
      return, 16
      break
    end
    12: begin
      return, 2
      break
    end
    13: begin
      return, 4
      break
    end
    14: begin
      return, 8
      break
    end
    15: begin
      return, 8
      break
    end    
    else: begin
      return, 4
    end
  endswitch

End

Function get_k2_position_filter, input_data, k

  compile_opt idl2
  
  ;dimension[列数,行数,波段数]
  dimension = size(input_data, /dimension)
  
  output_data = make_array(dimension[0], dimension[1], type=size(input_data, /type))
  
  for i=0, dimension[0]-1 do begin    
    for j=0, dimension[1]-1 do begin    
      
      ;原始值
      orgin_data = input_data[i, j, *]
      ;
      max_value = MAX(orgin_data, max_pos, /NAN)
      ;获取原始数据最大值之前的数据（包括最大值）
      data_0 = make_array(max_pos+1)
      for k = 0L, max_pos do begin
        data_0[k] = orgin_data[k]
      endfor
      ;
      k2_position = WHERE(data_0 GT k, count)
      ;如果没有大于k2的输出0
      if count eq 0 then k2_position = 0
      ;
      output_data[i, j] = k2_position[0]
          
    endfor
  endfor  
  
  return, output_data 

End

Pro get_k2_position_via_all, input_file_id, output_file, k

  compile_opt idl2
  
  envi_file_query, input_file_id, dims=dims, ns=ns, nl=nl, $
  nb=nb, data_type=data_type
  
  input_data = make_array(ns, nl, nb, type=data_type)
  ;输出数据为整型（Integer）
  output_data = make_array(ns, nl, type=2)
  for i=0, nb-1 do begin
    input_data[*, *, i] = envi_get_data(fid=input_file_id, $
    dims=dims, pos=i)
  endfor
  
  for i=0, ns-1 do begin
    for j=0, nl-1 do begin
      
      ;原始值
      orgin_data = input_data[i, j, *]
      ;原始值最大值及其位置
      max_value = MAX(orgin_data, max_pos, /NAN)
      ;获取原始数据最大值之前的数据（包括最大值）
      data_0 = make_array(max_pos+1)
      for k = 0L, max_pos do begin
        data_0[k] = orgin_data[k]
      endfor
      ;
      k2_position = WHERE(data_0 GT k, count)
      ;如果没有大于k2的输出0
      if count eq 0 then k2_position = 0
      ;
      output_data[i, j] = k2_position[0]
      
    endfor
  endfor
  
  map_info = envi_get_map_info(fid=input_file_id)
  envi_write_envi_file, output_data, out_name=output_file, $
  map_info=map_info  

End

Pro get_k2_position_via_tile, input_file_id, output_file, k

  compile_opt idl2

  envi_file_query, input_file_id, dims=dims, ns=ns, nl=nl, $
  nb=nb, data_type=data_type, interleave=interleave
  
  openw, unit, output_file, /get_lun
  
  tiles_num = 10
  tile_lines = [nl/tiles_num, nl/tiles_num, $
                nl/tiles_num, nl/tiles_num, $
                nl/tiles_num, nl/tiles_num, $
                nl/tiles_num, nl/tiles_num, $
                nl/tiles_num, nl-nl/tiles_num*9]
  print, tile_lines
  
  ;循环处理各个Tiles的内容
  for i=0, tiles_num-1 do begin
    ;获取Tiles[i]的内容
    data = make_array(ns, tile_lines[i], nb, type=data_type)
    for j=0, nb-1 do begin
      data[*, *, j] = envi_get_data(fid=input_file_id, $
      dims=[dims[0], dims[1], dims[2], i*nl/tiles_num, i*nl/tiles_num+tile_lines[i]-1], $
      pos=j)
    endfor
  
    ;##############################Tile处理部分############################################################
    output_data = get_k2_position_filter(data, k)
    ;#####################################################################################################    
  
    writeu, unit, output_data
  
    print, 'Tile', i
    
  endfor

  free_lun, unit
  
  map_info = envi_get_map_info(fid=input_file_id)
  
  envi_setup_head, fname=output_file, ns=ns, nl=nl, $
  nb=1, map_info=map_info, data_type=data_type, $
  offset=0, interleave=0, descrip='Test routine output', $
  /write, /open

End

Pro get_k2_position

  compile_opt idl2
  
  envi, /restore_base_save_files
  envi_batch_init
  
  input_file = dialog_pickfile(title='选择输入文件')
  if (input_file eq '') then return
  
  output_file = dialog_pickfile(title='选择输出文件', /WRITE)
  if (output_file eq '') then return  
  
  base = WIDGET_AUTO_BASE(TITLE='请输入参数K2')
  wp = WIDGET_PARAM(base, DT=4, FLOOR=0, PROMPT='k2：最大值前数值大于K2的第一个点位', UVALUE='param1', /AUTO_MANAGE)
  result = auto_wid_mng(base)
  if (result.accept eq 0) then return
  ;提取最大值前斜率大于K1的第一个点位
  K2 = result.param1
  
  envi_open_file, input_file, r_fid=fid
  if (fid eq -1) then begin
    return_dialog = dialog_message(['打开文件失败：', '该文件无法使用envi_open_file函数'], title='错误信息', /information)
    return
  endif
  envi_file_query, fid, dims=dims, ns=ns, nl=nl, nb=nb, data_type=data_type
  
  ;大于300M则进行分块处理
  file_size = ns*nl*nb*data_type_to_byte(data_type)
  max_file_size = 300*1024*1024
  
  if (file_size ge max_file_size) then begin
    
    print, '分块处理'
    get_k2_position_via_tile, fid, output_file, k2
    
  endif else begin
    
    print, '整体处理'
    get_k2_position_via_all, fid, output_file, k2
  
  endelse  
  
  envi_batch_exit  

End