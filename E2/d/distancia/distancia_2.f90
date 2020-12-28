! Programa que calcula a distância entre as gerações de dois valores ligeiramente diferentes.
	
	program distancia
	IMPLICIT NONE

	! Variáveis
	REAL*8 :: x			 ! densidade populacional (x)
	REAL*8 :: y			 ! densidade populacional xõ
	REAL*8 :: x_inicial		 ! densidade populacional inicial
	REAL*8 :: dx_inicial		 ! Variação da densidade populacional inicial
	REAL*8 :: E			 ! Diferença entre as densidades pop
	REAL*8 :: d 			 ! distância entre as duas densidades
	REAL*8 :: taxa 			 ! taxa de variação populacional atual (r)
	REAL*8, dimension(5) :: taxa_var ! Taxa de variação populacional (r)
	INTEGER*8 :: G			 ! número de gerações
	INTEGER*8 :: i			 ! contador
	INTEGER*8 :: j			 ! contador

	! Inicializando variáveis
	taxa_var(1) = 3.6d0
	taxa_var(2) = 3.7d0
	taxa_var(3) = 3.8d0
	taxa_var(4) = 3.9d0
	taxa_var(5) = 3.99d0
	E = 1.d-10


	! Abrindo arquivos para impressão de variáveis
	open(11, file = "dis_r1_x018.dat")
	open(12, file = "dis_r1_x036.dat")
	open(13, file = "dis_r1_x054.dat")
	open(14, file = "dis_r1_x072.dat")
	open(15, file = "dis_r1_x090.dat")
	open(21, file = "dis_r2_x018.dat")
	open(22, file = "dis_r2_x036.dat")
	open(23, file = "dis_r2_x054.dat")
	open(24, file = "dis_r2_x072.dat")
	open(25, file = "dis_r2_x090.dat")
	open(31, file = "dis_r3_x018.dat")
	open(32, file = "dis_r3_x036.dat")
	open(33, file = "dis_r3_x054.dat")
	open(34, file = "dis_r3_x072.dat")
	open(35, file = "dis_r3_x090.dat")
	open(41, file = "dis_r4_x018.dat")
	open(42, file = "dis_r4_x036.dat")
	open(43, file = "dis_r4_x054.dat")
	open(44, file = "dis_r4_x072.dat")
	open(45, file = "dis_r4_x090.dat")
	open(51, file = "dis_r5_x018.dat")
	open(52, file = "dis_r5_x036.dat")
	open(53, file = "dis_r5_x054.dat")
	open(54, file = "dis_r5_x072.dat")
	open(55, file = "dis_r5_x090.dat")
	

	! Queremos graficar a densidade para 3 taxas de vaiação diferentes
	DO i =1,5

		taxa  = taxa_var(i)
		dx_inicial = 0.18d0
		x_inicial = 0.d0
	
		! Queremos graficar a densidade para 5 x iniciais diferentes entre 0 e 1
		DO j=1,5
			
			! calculando valor inicial novo da densidade pop.
			x_inicial = x_inicial + dx_inicial
			x = x_inicial
			y = x_inicial + E
	
			! Calculando a variação da densidade populacional, utilizando um número arbitrário de gerações como geração máxima
			DO G = 1, 30
	
				x = taxa * x * (1.d0 - x)
				y = taxa * y * (1.d0 - y)

				d = ABS(y - x)
			
				write((10*i)+j,*)G,d
			enddo
		enddo
	enddo
	end program distancia
