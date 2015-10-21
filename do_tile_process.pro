;
;Name:
;
;Purpos:
;       using envi function(envi_get_data()) to do tile process!
;       you should write a function to process an one dimension array!
;Input:
;
;Output:
;
;Exmple:
;
;Theory:
;
;Author:
;       lyh/~_~/@2015.10.17
;
Pro do_tile_process, input, output, function_name=function_name, tiles_number=tiles_number, other_params=other_params

  compile_opt idl2
  
  envi, /restore_base_save_files
  envi_batch_init
  
  envi_open_file, input, r_fid=fid
  if (fid eq -1) then begin
    return_dialog = dialog_message(['打开文件失败：', '该文件无法使用envi_open_file函数'], title='错误信息', /information)
    return
  endif
  
  IF n_elements(function_name) LE 0 THEN begin
    message, 'you should provide a user function name to pararms!'
    return
  endif
  
  if n_elements(tiles_number) le 0 then tiles_number=10
  
  openw, unit, output, /get_lun
  
  ;get the information of inputfile
  envi_file_query, fid, dims=dims, ns=ns, nl=nl, nb=nb, data_type=data_type
  
  ;tiles_lines is an array that contain the lines number of each tiles!
  tiles_line = nl / tiles_number
  tiles_lines = make_array(tiles_number, /INTEGER)
  for i=0, tiles_number-1 do begin    
    tiles_lines[i] = (i ne tiles_number-1) ? tiles_line : (nl - i*tiles_line)    
  endfor
  
  print, '分块的行数：', tiles_lines
  
  output_bandnumber = nb
  
  ;timestamp
  t1 = systime(1)
  
  ;begin tiles process
  for i=0, tiles_number-1 do begin
    
    input_data = make_array(ns, tiles_lines[i], nb, type=data_type)
    for j=0, nb-1 do begin
      input_data[*, *, j] = envi_get_data(fid=fid, dims=[dims[0], dims[1], dims[2], $
                                                         i*tiles_line, i*tiles_line+tiles_lines[i]-1], pos=j)
    endfor
    
    ;dimensions[cols, rows, nb]
    dimensions = size(input_data, /DIMENSIONS)
    
    ;this part must be modify! because this part will out the same dimensions of input!
    ;call user function in here!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    for rows=0, dimensions[1]-1 do begin
      for cols=0, dimensions[0]-1 do begin
        
        curve = reform(input_data[cols, rows, *])
        if (n_elements(other_params) ne 0) then begin
          call_procedure, function_name, curve, out_curve, other_params
        endif else begin
          call_procedure, function_name, curve, out_curve
        endelse 
        
        
        ;determine the output_bandnumber
        if (rows+cols eq 0) then output_bandnumber = n_elements(out_curve)
        
        input_data[cols, rows, 0:(output_bandnumber-1)] = out_curve
        
      endfor
    endfor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;change output_data dimensions
    input_data = input_data[*, *, 0:(output_bandnumber-1)]
     
    ;convert to BIL!
    if (size(input_data, /N_DIMENSIONS) eq 3) then $
      input_data = transpose(temporary(input_data), [0, 2, 1])
    ;write input_data to outputfile
    writeu, unit, input_data
    
  endfor
  
  ;timestamp
  print, systime(1)-t1
  
  free_lun, unit
  
  map_info = envi_get_map_info(fid=fid)
  envi_setup_head, fname=output, ns=ns, nl=nl, nb=output_bandnumber, map_info=map_info, $ 
  data_type=data_type, offset=0, interleave=1, $
  descrip='do_tile_process: out_put', /write, /open  
  envi_batch_exit
  
End
;
Pro my_tile_process_function, input, output, other_params
  
  compile_opt idl2
   
  
  output = input[0:10]
  
End