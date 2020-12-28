	! Programa que constroí o diagrama de bifurcação do mapa logístico do crescimento de uma população

	program bifurcacao
	IMPLICIT NONE

	! Variáveis
	REAL*8 :: pop ! Densidade populacional (x)
	REAL*8 :: pop_inicial !densidade populacional inicial
	REAL*8 :: taxa ! taxa de variação populacional
	REAL*8 :: var_taxa ! Variação da taxa populacional
	INTEGER*8 :: Nconverge ! número de iterações para ter convergência para x*
	INTEGER*8 :: Ncoleta ! número de pontos coletados para cada iteração
	INTEGER*8 :: i ! contador
	INTEGER*8 :: j ! contador

	! Abrindo arquivos para impressão de variáveis
	open(20, file = "bifurcacao.dat")
	
	! Inicializando variáveis
	var_taxa = 0.001d0
	Nconverge = 50
	Ncoleta = 1000
	taxa = 2.8d0	

	! Começamos do valor que seria x* não convencional, diminuindo as iterações necessárias para convergir
	pop_inicial = 1 - (1/taxa)
	pop = pop_inicial

	do while (taxa < 3.99)
		
		! Iteramos o mapa até obtermos a convergência para x*
		do i = 1,Nconverge

			pop = taxa * pop * (1 - pop)		

		enddo

		! Coletamos então o ponto x* (coletamos vários pois não sabemos quantos pontos divergentes temos para cada r)
		do j = 1, Ncoleta

			write(20,*)taxa, pop
			pop = taxa * pop * ( 1 - pop)

		enddo

		! Iteramos a taxa de variação:
		taxa = taxa + var_taxa

	enddo

	end program bifurcacao
