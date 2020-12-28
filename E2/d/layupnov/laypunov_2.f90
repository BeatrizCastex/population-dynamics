! Programa que calcula a estimativa do expoente de Lyapunov pelo ln|G'(xo)| -> G'(xo) = r (1 - 2x)
	
	program lyapunov
	IMPLICIT NONE
	
	! Variáveis
	REAL*8 :: pop	       ! densidade populacional (x)
	REAL*8 :: pop_inicial  ! densidade populacional inicial
	REAL*8 :: dpop_inicial ! Variação da densidade populacional inicial
	REAL*8 :: pop_linha    ! derivada da densidade popuçacional
	REAL*8 :: taxa         ! taxa de variação populacional atual
	INTEGER*8 :: n         ! número de gerações
	INTEGER*8 :: i         ! contador

	! Abrindo arquivos para impressão de variáveis
	open(11, file = "lyapunov_x018.dat")
	open(12, file = "lyapunov_x036.dat")
	open(13, file = "lyapunov_x054.dat")
	open(14, file = "lyapunov_x072.dat")
	open(15, file = "lyapunov_x090.dat")
	
	! Inicializando variáveis
	taxa = 2.5d0
	dpop_inicial = 0.18d0
	pop_inicial = 0.d0

	! Queremos graficar a derivada da densidade para 5 x iniciais diferentes entre 0 e 1
	DO i = 1,5
			
		! calculando valor inicial novo da densidade pop.
		pop_inicial = pop_inicial + dpop_inicial
		pop = pop_inicial
	
		! Calculando a derivada da variação da densidade populacional, utilizando um número arbitrário de gerações como geração máxima
		DO n = 1, 30
	
			pop = taxa * pop * (1.d0 - pop)
			pop_linha = taxa * (1 - (2.d0 * pop))

			write(10+i,*)n,LOG(ABS(pop_linha))
		enddo
	enddo

	end program lyapunov
