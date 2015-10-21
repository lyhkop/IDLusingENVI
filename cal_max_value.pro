Pro cal_max_value

  compile_opt idl2
  
  envi, /RESTORE_BASE_SAVE_FILES
  envi_batch_init
  
  envi_select, fid=fid, dims=dims, /file_only, /no_dims, title='请选择进行最大值合成的数据'
  if (fid eq -1) then return
  
  envi_file_query, fid, nb=nb, ns=ns, nl=nl, dims=dims
  
  output_file = dialog_pickfile(title='请选择输出文件', /write)
  if (output_file eq '') then return
  
  input_fids=lonarr(nb)
  input_pos=lonarr(nb)
  input_dims=lonarr(5, nb)
  for index=0l, nb-1 do begin
    input_fids[index]=fid
    input_dims[*, index]=dims
    input_pos[index]=index
  endfor
  
  bands=strarr(nb)
  for index=0l, nb-1 do begin
    bands[index]='b'+strtrim(string(index), 1)
  endfor
  exp = 'b0'
  for index=0l, nb-2 do begin
    exp= exp + '>' + bands[index+1] 
  endfor
  
  envi_doit, 'math_doit', fid=input_fids, pos=input_pos, dims=input_dims, out_name=output_file, out_bname='mvc', $
  exp=exp
  
  envi_batch_exit
  
End