ForpulseDIA<-function(dia_engine,input_file_list){
  switch(dia_engine,
    "OpenSWATH"=Openswath4PluseDIA(input_file_list[1]),
    "Spectronaut"=Spectronaut4PluseDIA(input_file_list),
    "DIA_NN"=DIA_NN4PluseDIA()
  )
}