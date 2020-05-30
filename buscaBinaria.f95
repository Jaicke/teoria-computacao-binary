function buscaBinaria(v, x, n)
  integer, intent(in) :: x, n
  integer, intent(in) :: v(0:4)
  integer :: limInf, limSup, meio
  integer :: result
  
  limInf = 0
  limSup = n - 1
  
  do while (limInf <= limSup)
    meio = (limInf + limSup) / 2
    
    if(v(meio) == x) then
        result = meio
    else
        result = -1
    end if
    
    if(v(meio) < x) then
        limInf = meio + 1
    else
        limSup = meio - 1
    end if
  end do

  print *, result
end function

program main
  integer :: arr(0:4)
  integer :: arrTamanho, item
  real :: tempoInicio, tempoFim
  arr = [1,2,3,4,5]
  arrTamanho = 5
  item = 5
    
  call cpu_time (tempoInicio)   
  result = buscaBinaria(arr, 5, arrTamanho)
  call cpu_time (tempoFim)
  
  print *, tempoFim - tempoInicio
end program main
