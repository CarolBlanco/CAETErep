module reproduction
    implicit none
    private
    public :: repro

contains

    subroutine repro(awood, temp, prec, nppa, height1, seed_pft, seed_bank) ! seed_bank, new_seed_bank)
        use global_par

        ! Declaração das variáveis de entrada
        real(r_8), intent(in) :: height1
        real(r_4), intent(in) :: nppa
        real(r_8), intent(in) :: awood
        real(r_4), intent(in) :: temp, prec
        integer(i_4), intent(in) :: seed_pft


        ! Declaração das variáveis de saída
        integer(i_4), intent(out) :: seed_bank
        !real(r_8), intent(out) :: new_seed_bank

        ! Variáveis internas
        real(r_8) :: height
        real(r_8) :: npp_rep
        real(r_8) :: seed_mass_log
        integer(i_4) :: germin 
        integer(i_4) :: n_seed 
        real(r_8) :: seed_mass

        !INICIALIZE OUTPUTS VARIABLES
        germin = 0.0D0

        ! CONDIÇÕES PARA INICIAR A PRODUÇÃO DE SEMENTES
        height = height1  ! Altura da planta em metros
        npp_rep = nppa * 0.04  ! 4% do NPP disponível para reprodução

        !Verifica as condicoes para producao de sementes
        if (temp .lt. 23.0 .and. temp .gt. 33.0 .and. &
        prec .lt. 60.0 .and. prec .gt. 200.0 .and. &
        nppa .le. 0.0 .and. awood .le. 0.0D0 .and. height1 .le. 5.0) then

            ! Se as condicoes nao forem atendidas, nao ha producao de sementes
            seed_mass_log = 0.0
            seed_mass = 0.0
            n_seed = 0
        else
            ! Calculo da massa da semente em escala logaritmica
            seed_mass_log = (0.039 * height) + 1.1951

            ! Converte a massa da semente para escala normal (linear)
            seed_mass = (10.0 ** seed_mass_log) / 1.0D6

            ! Calcula o numero de sementes
            n_seed = int(npp_rep / seed_mass)
        endif

        ! Atualizacao do banco de sementes
        if (n_seed .lt. 0) then
            germin = 0.0D0
            seed_bank = seed_pft
        else
            germin = seed_bank * 0.3
            seed_bank = ((seed_pft + n_seed) - germin)
        endif

        print*, 'seed_bank', seed_bank

        ! !print *, "valor de npp_rep:", npp_rep
        ! !print *, "valor de nppa", nppa
        ! if (awood .gt. 0.0D0 .and. height1 .gt. 5.0 ) then
        !     !print*, 'ALTURA POSITIVA = PRODUÇÃO'

        !     seed_mass_log = ((0.039 * height) + 1.1951)
        !     !seed_mass_log = 0.0
        
        !     !print *, "seed_mass_log", seed_mass_log

        !     !Converte a massa da semente de escala logarítmica para normal (linear)
        !     seed_mass = ((10.0 ** seed_mass_log) / 1.0D6)
        !     !print *, "seed_mass", seed_mass

        !     ! Imprime a massa da semente
        !     !print *, "Altura da planta:", height, "m, Massa da semente:", seed_mass, "kg"

        !     ! Calculando o número de sementes
        !     n_seed = int(npp_rep / seed_mass)
        !     !n_seed = int(n_seed)  ! Garantindo que o número de sementes seja um valor inteiro

        ! else
        !     ! Se a altura for zero, não há produção de sementes
        !     seed_mass = 0.0
        !     n_seed = 0

        !     if (n_seed .gt. 0) then
        !         seed_bank = (seed_bank + n_seed)
        !         !print*, 'seed_in', seed_in
        !         !print*, 'seed_bank on module - before germin', seed_bank
        !         !print*, 'numero de sementes', n_seed
        !         germin = (seed_bank*0.3)
        !         seed_bank = seed_bank - germin
        !         !print*, 'seed_bank on module - after germin', seed_bank
        !     else
        !         seed_bank = seed_bank !Não há incremento no banco de sementes
        !         !print*, 'seed_bank on module - no germin', seed_bank
        !         germin = 0.0D0
        !     endif 

        ! endif

        !print *, "n_seed:", n_seed

        !CONSTRUÇÃO DO BANCO DE SEMENTES & GERMINAÇÃO
        !! DECAY DO BANCO DE SEMENTES DIÁRIO
        !int_seed_bank(ri) = int(int_seed_bank(ri) * 0.99)
        !print *, "Banco de sementes após decomposição diária:", int_seed_bank(ri)



        ! Atualização do banco de sementes
        !if (n_seed .gt. 0) then
        !    new_seed_bank = seed_bank + n_seed
        !else
        !    new_seed_bank = seed_bank  ! Não altera se não houver produção
        !endif


        ! Atualizando o banco de sementes
        !new_seed_bank = seed_bank + n_seed
        !seed_bank = new_seed_bank

        ! Evitar que seed_bank assuma valores não realistas
        !if (seed_bank < 0) then
        !    seed_bank = 0
        !endif

        ! Imprime para depuração
        !print *, "Tamanho do banco de sementes após a produção_module_repro:", new_seed_bank

    end subroutine repro


end module reproduction
