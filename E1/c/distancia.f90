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
	REAL*8, dimension(3) :: taxa_var ! Taxa de variação populacional (r)
	INTEGER*8 :: G			 ! número de gerações
	INTEGER*8 :: i			 ! contador
	INTEGER*8 :: j			 ! contador

	! Inicializando variáveis
	taxa_var(1) = 1.5d0
	taxa_var(2) = 2.5d0
	taxa_var(3) = 2.7d0
	E = 0.001d0


	! Abrindo arquivos para impressão de variáveis
	open(11, file = "distancia_r1.5_x018.dat")
	open(12, file = "distancia_r1.5_x036.dat")
	open(13, file = "distancia_r1.5_x054.dat")
	open(14, file = "distancia_r1.5_x072.dat")
	open(15, file = "distancia_r1.5_x090.dat")
	open(21, file = "distancia_r2.5_x018.dat")
	open(22, file = "distancia_r2.5_x036.dat")
	open(23, file = "distancia_r2.5_x054.dat")
	open(24, file = "distancia_r2.5_x072.dat")
	open(25, file = "distancia_r2.5_x090.dat")
	open(31, file = "distancia_r2.7_x018.dat")
	open(32, file = "distancia_r2.7_x036.dat")
	open(33, file = "distancia_r2.7_x054.dat")
	open(34, file = "distancia_r2.7_x072.dat")
	open(35, file = "distancia_r2.7_x090.dat")
	

	! Queremos graficar a densidade para 3 taxas de vaiação diferentes
	DO i =1,3

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
