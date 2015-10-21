Pro Spatialsubset, input_file_fid, shp_file_name, output_file_name
  
  compile_opt idl2
  
  envi_file_query, input_file_fid, ns=ns, nl=nl, nb=nb, dims=dims, BNAMES=bnames
  
  oshp = obj_new('idlffshape', shp_file_name)
  
  oshp->GetProperty, N_ENTITIES=n_ent, ATTRIBUTE_INFO=attr_info, N_ATTRIBUTES=n_attr, ENTITY_TYPE=ent_type
  
  roi_shp = lonarr(n_ent)
  
  for ishp=0, n_ent-1 do begin
    
    entitie = oshp->GetEntity(ishp)
    if entitie.shape_type eq 5 then begin
      
      record = *(entitie.vertices)
      
      envi_convert_file_coordinates, input_file_fid, xmap, ymap, record[0,*], record[1,*]
      
      roi_shp[ishp] = envi_create_roi(ns=ns, nl=nl)
      envi_define_roi, roi_shp[ishp], /polygon, xpts=reform(xmap), ypts=reform(ymap)
      
      if (ishp eq 0) then begin
        
        xmin = round(min(xmap, max=xmax))
        ymin = round(min(ymap, max=ymax))
      
      endif else begin
        
        xmin = xmin < round(min(xmap))
        xmax = xmax > round(max(xmap))
        ymin = ymin < round(min(ymap))
        ymax = ymax > round(max(ymap))  
  
      endelse    
    endif 
    oshp->DestroyEntity, entitie
  endfor
  
  xmin = xmin > 0
  xmax = xmax < ns - 1
  ymin = ymin > 0
  ymax = ymax < nl - 1
  
  envi_mask_doit, and_or=1, /in_memory, roi_ids=roi_shp, ns=ns, nl=nl, /inside, r_fid=m_fid
  
  out_dims = [-1,xmin,xmax,ymin,ymax]
  
  envi_mask_apply_doit, fid=input_file_fid, pos=indgen(nb), dims=out_dims, $
  m_fid=m_fid, m_pos=[0], value=0, $
  out_bname= bnames+'_mask',in_memory=0, out_name=output_file_name, r_fid=r_fid
  
  envi_file_mng, id=m_fid, /remove

End

Pro Subset_via_shp
  
  compile_opt idl2
  
  envi, /restore_base_save_files
  
  envi_batch_init, log_file='batch.log'
  
  input_files_path = dialog_pickfile(title='请选择待裁剪文件的路径', /directory)
  if (input_files_path eq '') then begin
   return
  endif
  input_files = file_search(input_files_path, '*.ld3', count=input_files_count)
  if (input_files_count eq 0) then begin 
    dialog_return = dialog_message([input_files_path, ':路径下没有文件.ld3'], title='警告信息！', /information)
    return
  endif
  
  
  input_shp = dialog_pickfile(title='请选择用于裁剪的面文件', filter='*.shp')
  if (input_shp eq '') then begin
    return
  endif
  
  
  
  output_files_path = dialog_pickfile(title='请选择输出文件的路径', /directory)
  if (output_files_path eq '') then begin
    return
  endif
  
  for i=0, input_files_count-1 do begin
  
;    catch, error_status
;    if (error_status ne 0) then begin
;      
;      print, '打开文件失败', !error_status.MSG
;      ;处理错误
;      
;      continue
;    endif
    
    input_file = input_files[i]
    envi_open_file, input_file, r_fid=input_file_id, /no_interactive_query, /no_realize
    if (input_file_id eq -1) then begin
      print, 'use envi_open_file process to open ', input_file, ' failed!'
      continue
    endif
    
    output_file_name = output_files_path + file_basename(input_file, '.ld3') + '_mask.ld3'
    
    Spatialsubset, input_file_id, input_shp, output_file_name
    
  endfor
  
  envi_batch_exit
  
End