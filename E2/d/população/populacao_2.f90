	! Programa qua calcula a densidade populacional de uma população sem predadores em função do número de gerações para 5 densidades iniciais diferentes utilizando diferentes taxas de variação populacional
	
	program populacao
	IMPLICIT NONE

	! Variáveis
	REAL*8 :: pop 			 ! densidade populacional (x)
	REAL*8 :: pop_inicial   	 !densidade populacional inicial
	REAL*8 :: dpop_inicial 		 ! Variação da densidade populacional inicial
	REAL*8 :: taxa 			 ! taxa de variação populacional atual
	REAL*8, dimension(5) :: taxa_var ! Taxa de variação populacional
	INTEGER*8 :: G 			 ! número de gerações
	INTEGER*8 :: i 			 ! contador
	INTEGER*8 :: j 			 ! contador

	! Inicializando variáveis
	taxa_var(1) = 3.6d0
	taxa_var(2) = 3.7d0
	taxa_var(3) = 3.8d0
	taxa_var(4) = 3.9d0
	taxa_var(5) = 3.99d0
	

	! Abrindo arquivos para impressão de variáveis
	open(11, file = "populacao_r1_x018.dat")
	open(12, file = "populacao_r1_x036.dat")
	open(13, file = "populacao_r1_x054.dat")
	open(14, file = "populacao_r1_x072.dat")
	open(15, file = "populacao_r1_x090.dat")
	open(21, file = "populacao_r2_x018.dat")
	open(22, file = "populacao_r2_x036.dat")
	open(23, file = "populacao_r2_x054.dat")
	open(24, file = "populacao_r2_x072.dat")
	open(25, file = "populacao_r2_x090.dat")
	open(31, file = "populacao_r3_x018.dat")
	open(32, file = "populacao_r3_x036.dat")
	open(33, file = "populacao_r3_x054.dat")
	open(34, file = "populacao_r3_x072.dat")
	open(35, file = "populacao_r3_x090.dat")
	open(41, file = "populacao_r4_x018.dat")
	open(42, file = "populacao_r4_x036.dat")
	open(43, file = "populacao_r4_x054.dat")
	open(44, file = "populacao_r4_x072.dat")
	open(45, file = "populacao_r4_x090.dat")
	open(51, file = "populacao_r5_x018.dat")
	open(52, file = "populacao_r5_x036.dat")
	open(53, file = "populacao_r5_x054.dat")
	open(54, file = "populacao_r5_x072.dat")
	open(55, file = "populacao_r5_x090.dat")
	

	! Queremos graficar a densidade para 3 taxas de vaiação diferentes
	DO i =1,5

		taxa  = taxa_var(i)
		dpop_inicial = 0.18d0
		pop_inicial = 0.d0
	
		! Queremos graficar a densidade para 5 x iniciais diferentes entre 0 e 1
		DO j=1,5
			
			! calculando valor inicial novo da densidade pop.
			pop_inicial = pop_inicial + dpop_inicial
			pop = pop_inicial
	
			! Calculando a variação da densidade populacional, utilizando um número arbitrário de gerações como geração máxima
			DO G = 1, 30
	
				pop = taxa * pop * (1.d0 - pop)
			
				write((10*i)+j,*)G,pop
		enddo
	enddo

	end program populacao
