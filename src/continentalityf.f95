! Predictors calculation on the CRU rectangular grid.
! https://www.avrahamadler.com/2018/12/09/the-need-for-speed-part-1-building-an-r-package-with-fortran/

module continentality
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: compute_continentality_f

contains

  ! ################## !
  subroutine compute_continentality_f(NX, NY, NBmois, X1, X2, Y1, Y2, dlon, dlat, lon, lat, Udsc, Vdsc, mask, Aco, Dco) &
      bind(C, name = "compute_continentality_f_")
  ! ################## !


    !$ USE OMP_LIB
    ! Tof declar
    ! Inputs/Outputs
    integer(kind = c_int), intent(in), value ::  NX, NY, NBmois   
    real(kind = c_double), intent(in), value :: X1, X2, Y1, Y2, dlon, dlat
    integer(kind = c_int), intent(in), dimension(NX,NY) ::  mask
    real(kind = c_double), intent(in), dimension(NX,NY,NBmois) ::  Udsc, Vdsc 
    real(kind = c_double), intent(in), dimension(NX) :: lon                          ! Vecteur longitudes CRU
    real(kind = c_double), intent(in), dimension(NY) :: lat                          ! Vecteur latitudes CRU
    real(kind = c_double), intent(out), dimension(NX,NY) :: Dco
    real(kind = c_double), intent(out), dimension(NX,NY,NBmois) :: Aco 
    ! Intermediate variables 
    real, parameter :: Rearth=(6378.137+6356.752)/2.0   ! Rayon terrestre moyen (entre équateur et pôles) en Km
    real, parameter :: RPI=3.141592653589793238462643383279502884197
    integer :: n, i, j, ii, jj, nbrptsy
    integer :: flpath
    integer :: NX1, NX2, NY1, NY2
    integer, dimension(NY) :: nbrptsx
    logical, dimension(NX,NY) :: invest ! tableau T ou F si investigation sur coord i,j
    logical, dimension(NX,NY,NBmois) :: invest3D ! tableau T ou F si investigation sur coord i,j
    real, dimension(NX,NY) :: distpts ! distance entre pts et cible
    real, dimension(NX,NY) :: latrad,lonrad ! lon et lat des points en radians
    integer, dimension(nx,ny) :: imin,imax,jmin,jmax ! coord min et max d'ivestigation pour chaue point
    real, dimension(nx,ny) :: azimuth
    real, dimension(nx,ny,NBmois) :: adwind ! vent selon l'azimuth
    real, dimension(nx,ny,NBmois) :: azwind ! vent selon l'azimuth
    real, dimension(nx,ny,NBmois) :: azwinddist ! vent selon azimuth * distance entre les 2 points
    real, dimension(nx,ny,NBmois) :: zerotab    ! tableau de zero
    integer, dimension(nx,ny,NBmois) :: mask3D ! masque Terre/Mer sur variable 3D avec 12 mois
    real, dimension(NY) :: deltaX
    real :: deltaY
    !!$! calcul de la continentalite advective Aco : Aco = (ventmax-vent)*facto
    real :: ventmax=12. ! vent max pour lequel la continentalite est minimum
    !!$! facteur pour calcul Aco : (ventmax-vent)*distmax/100 = factaco avec distmax= distance ou conti=100
    !!$! exemple vent=0 : Aco=100 a partir de 200km : factaco=ventmax*200km/100=50
    real :: factAco=100.  ! facteur pour calcul ACO
    real :: factAco2=8.5 ! facteur pour calcul ACO  ! augmente sensibilite au vent : si azWind=30 Aco=100 à 1250 km
    ! facteur de calcul Dco : 100% pour 500km
    real :: factDco=500./100.

    integer :: imin_invest,imax_invest,jmin_invest,jmax_invest ! zone de recherche etendue (zone d'etude + flpath)

    ! variables parallelisation openMP
    integer :: rang, nb_taches
    logical :: paral
    integer :: i_min, i_max

    ! Fenetre de calcul (pas sur toute la grille => trop long !)
    print*, 'Definition de la fenetre de calcul (Valeurs / indices) :'
    print*,'NX, NY, NBmois :', NX, NY, NBmois
    print*,'X1, X2, Y1, Y2 :', X1, X2, Y1, Y2
    print*,'Udsc[1], Udsc[2], Udsc[3] :', Udsc(1, 1, 1), Udsc(2, 1, 1), Udsc(3, 1, 1)
    print*,'Udsc[10, 9, 5]:', Udsc(10, 9, 5)
    print*,'Vdsc[1], Vdsc[2], Vdsc[3] :', Vdsc(1, 1, 1), Vdsc(2, 1, 1), Vdsc(3, 1, 1)
    print*,'Vdsc[10, 9]:', Vdsc(10, 9, 5)
    print*,'mask[1], mask[2], mask[3] :', mask(1, 1), mask(2, 1), mask(3, 1)
    print*,'mask[10, 9]:', mask(10,9)
    print*,'dlon, dlat:', dlon, dlat 
    print*,'lon(1)', lon(1) 
    NX1=abs(floor((X1-lon(1)-dlon/2.0)/dlon))
    NX2=abs(ceiling((X2-lon(1)-dlon/2.0)/dlon))
    NY1=abs(floor((Y1-lat(1)-dlat/2.0)/dlat))
    NY2=abs(ceiling((Y2-lat(1)-dlat/2.0)/dlat))

    print*,'lon(nx1) : ',lon(nx1)
    print*,'lon(nx2) : ',lon(nx2)
    print*,'lat(ny1) : ',lat(ny1)
    print*,'lat(ny2) : ',lat(ny2)

    PRINT*, 'LON 1 : ',X1,' / indice : ',NX1
    PRINT*, 'LON 2 : ',X2,' / indice : ',NX2
    PRINT*, 'LAT 1 : ',Y1,' / indice : ',NY1
    PRINT*, 'LAT 2 : ',Y2,' / indice : ',NY2

    deltaX = Rearth * cos(lat(:)*RPI/180.0) * dlon * RPI/180.0
    deltaY = Rearth * dlat * RPI/180.0


    print*,'entree dans CALCUL_PREDICTORS'
    zerotab(:,:,:)=0.
    Aco(:,:,:)=0.
    Dco(:,:)=0.
    azWinddist(:,:,:)=0.

    do n=1,NBmois
      mask3D(:,:,n)=mask(:,:)
    enddo

    ! zone de travail :
    print*,'NX1,NX2',NX1,NX2
    print*,'NY1,NY2',NY1,NY2

    ! Distance d'investigation (Km) : attention nombre entier !
    ! exemple : flpath= 3000. > rapide
    flpath = 1500 ! assez rapide
    !  flpath = 500 ! rapide
    ! nbr pts de grille nord/sud de la zone d'investigation
    nbrptsy = floor(flpath/deltaY)+1
    print*,'flpath, deltaY, nbrptsy :', flpath, deltaY, nbrptsy

    !-----------------------------------------------------
    !
    !                         j+nbrptsy
    !
    !
    ! i-nbrptsx(j)            i,j             i+nbrptsx(j)
    !
    !
    !
    !                         j-nbrptsy
    !
    !------------------------------------------------------
    print*,'flpath = ',flpath
    !$OMP PARALLEL
    !$ paral = OMP_IN_PARALLEL()
    !$ print*,'flpath vaut : ',flpath ,'; paral vaut ', paral
    !$ print *, 'Hello from process:', omp_get_thread_num()
    !$OMP END PARALLEL

    do j=NY1,NY2
      nbrptsx(j)=floor(flpath/deltaX(j))+1
    enddo

    do j=1,NY
      do i=1,NX
        ! passage en 0-360 pour la lon et en radian :
        if (lon(i).lt.0.) then
          lonrad(i,j)=(360.+lon(i))*RPI/180.
        else
          lonrad(i,j)=lon(i)*RPI/180.
        endif
        latrad(i,j)=lat(j)*RPI/180.
      enddo
    enddo

    ! Norme du vent en chaque point
    adWind(:,:,:)= sqrt(Udsc(:,:,:)**2 + Vdsc(:,:,:)**2)
    ! definition de la zone de recherche incluant la zone d'investigation (fonction de flpath)
    imin_invest=max(1,NX1-nbrptsx(NY1))
    imax_invest=min(NX,NX2+nbrptsx(NY2))
    jmin_invest=max(1,NY1-nbrptsy)
    jmax_invest=min(NY,NY2+nbrptsy)
    print*,'imin_invest imax_invest',imin_invest, imax_invest
    print*,'jmin_invest jmax_invest',jmin_invest, jmax_invest

    ! Boucle pour tous les points de la grille
    ! ----------------------------------------
    !$OMP PARALLEL PRIVATE(rang,nb_taches,i_min,i_max,azimuth,distpts,azwind,azWinddist,invest)
    !$ rang=OMP_GET_THREAD_NUM()
    !$ nb_taches=OMP_GET_NUM_THREADS()
    !$ i_min=NY1
    !$ i_max=NY2
    !$OMP DO SCHEDULE(STATIC,NY/nb_taches)
    do j=NY1,NY2
      write(*, fmt="(a)", advance="no") "."
      do i=NX1,NX2
        azimuth(:,:)=0.
        distpts(:,:)=0.
        azwind(:,:,:)=0.
        azWinddist(:,:,:)=0.
        ! nouvelle version du code
        ! On definie la distance de recherche : flpath
        ! calcul de la distance des points par rapport au point etudié
        ! calcul de l'angle par rapport au point etudié
        ! il n'y a plus de pas d'investigation, on travail à la résolution des données
        ! calcul des coordonnees des points extremes d'investigation :
        imin(i,j)=max(1,i-nbrptsx(j))
        imax(i,j)=min(NX,i+nbrptsx(j))
        jmin(i,j)=max(1,j-nbrptsy)
        jmax(i,j)=min(NY,j+nbrptsy)
        ! masque zone d'investigation pour chaque point de grille :
        invest(:,:)=.false.
        invest(imin(i,j):imax(i,j),jmin(i,j):jmax(i,j))=.true.
        ! calcul du masque invest sur 3D:

        ! calcul des variables hors du where
        ! Azimuth entre point de ref et point cible
        ! formule de calcul distance et azimuth entre 2 pts fct lon/lat
        ! http://www.movable-type.co.uk/scripts/latlong.html

        !!!!! Version avec boucle
        do jj=jmin_invest,jmax_invest
          do ii=imin_invest,imax_invest
            if (invest(ii,jj).eqv..true.) then 
              ! azimuth sens inverse !!!
              azimuth(ii,jj) = atan2(sin(lonrad(i,j)-lonrad(ii,jj))*cos(latrad(i,j)), &
                cos(latrad(ii,jj))*sin(latrad(i,j))-sin(latrad(ii,jj))*cos(latrad(i,j))*cos(lonrad(i,j)-lonrad(ii,jj)))
              distpts(ii,jj) = acos(sin(latrad(ii,jj))*sin(latrad(i,j)) + &
                cos(latrad(ii,jj))*cos(latrad(i,j))*cos(lonrad(i,j)-lonrad(ii,jj))) * Rearth
              ! vent projecté sur l'angle de l'azimuth :
                do n=1,NBmois
                  azWind(ii,jj,n)=cos(RPI/2.-azimuth(ii,jj))*Udsc(ii,jj,n) + cos(azimuth(ii,jj))*Vdsc(ii,jj,n)
                  ! calcul du coef azWinddist qui varie entre 0 et 100 :
                  azWinddist(ii,jj,n)=min(max((ventmax-azWind(ii,jj,n)),0.)*distpts(ii,jj)/ &
                    (factAco+(max(azWind(ii,jj,n),0.)*factAco2)),100.)
                  azWinddist(ii,jj,n)=max(0.,azWinddist(ii,jj,n))
                enddo
            endif
          enddo
        enddo

        do n=1,NBmois
          where (invest(:,:))
            azWinddist(:,:,n)=max(zerotab(:,:,n),azWinddist(:,:,n))
          endwhere
        enddo

        if (mask(i,j)==0) then
          distpts(i,j)=0.
          azWinddist(i,j,:)=0.
        endif 

        ! Dco est minval de distpts sur zone d'investigation et points mer (mask=0)
        !3D        Dco(i,j,1)=minval(distpts(:,:), mask=(invest(:,:)==.true..and.mask(:,:)==0)) / factDco
        Dco(i,j)=minval(distpts(:,:), mask=((invest(:,:).eqv..true.).and.(mask(:,:).eq.0))) / factDco
        ! print*,"Dco(", i,",",j,") = ", Dco(i,j)  
        ! Dco est compris entre 0 et 100
        where (Dco(:,:).GE.100..and.invest(:,:))
          Dco(:,:)=100.
        endwhere

        ! Aco : valeur mini de azWinddist dans zone investigation et si océan
        do n=1,NBmois
          Aco(i,j,n)=minval(azWinddist(:,:,n), mask=((invest(:,:).eqv..true.).and.(mask3D(:,:,n).eq.0)))
        enddo
      enddo
    enddo
    !$OMP END DO NOWAIT
    !$ print*,"boucle PREDICTEURS Rang : ",rang,"; i_min :",i_min,"; i_max :",i_max
    !$OMP END PARALLEL

    print*,'FIN DU CALCUL DES PREDICTEURS'
    PRINT*,'ACO (min/max) :',MINVAL(Aco),MAXVAL(Aco)
    PRINT*,'DCO (min/max) :',MINVAL(Dco),MAXVAL(Dco)

  end subroutine compute_continentality_f


end module continentality
