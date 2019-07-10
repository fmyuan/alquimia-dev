module alquimia_fortran_interface_mod


  use AlquimiaContainers_module, only : AlquimiaSizes,AlquimiaProblemMetaData,AlquimiaProperties,&
           AlquimiaState,AlquimiaAuxiliaryData,AlquimiaAuxiliaryOutputData, AlquimiaEngineStatus,&
           AlquimiaGeochemicalCondition,AlquimiaEngineFunctionality
  use iso_c_binding, only : c_funptr
           
  implicit none
  
  type, public, bind(c) :: AlquimiaInterface
    
    type(c_funptr) :: Setup
    type(c_funptr) :: Shutdown
    type(c_funptr) :: ProcessCondition
    type(c_funptr) :: ReactionStepOperatorSplit
    type(c_funptr) :: GetAuxiliaryOutput
    type(c_funptr) :: GetProblemMetaData
  
  end type AlquimiaInterface
   

  
  interface
    subroutine CreateAlquimiaInterface(engine_name, alq_interface, status) bind(C, name='CreateAlquimiaInterface')
      use AlquimiaContainers_module, only : AlquimiaEngineStatus
      use iso_C_binding, only: c_char
      IMPORT
      implicit none
      character(kind=c_char) :: engine_name(*)
      type(AlquimiaInterface)      :: alq_interface
      type(AlquimiaEngineStatus)   :: status
    end subroutine
  end interface
  
  ! Memory allocation subroutines
  interface
    subroutine AllocateAlquimiaEngineStatus(status) bind(C, name='AllocateAlquimiaEngineStatus')
      use AlquimiaContainers_module, only : AlquimiaEngineStatus
      implicit none
      type(AlquimiaEngineStatus) :: status
    end subroutine
  end interface
  interface
    subroutine FreeAlquimiaEngineStatus(status) bind(C, name='FreeAlquimiaEngineStatus')
      use AlquimiaContainers_module, only : AlquimiaEngineStatus
      implicit none
      type(AlquimiaEngineStatus) :: status
    end subroutine
  end interface
  
  interface
    subroutine AllocateAlquimiaProblemMetaData(sizes, meta_data) bind(C, name='AllocateAlquimiaProblemMetaData')
      use AlquimiaContainers_module, only : AlquimiaSizes, AlquimiaProblemMetaData
      implicit none
      type(AlquimiaSizes) :: sizes
      type(AlquimiaProblemMetaData) :: meta_data
    end subroutine
  end interface
  interface
    subroutine FreeAlquimiaProblemMetaData(meta_data) bind(C, name='FreeAlquimiaProblemMetaData')
      use AlquimiaContainers_module, only : AlquimiaProblemMetaData
      implicit none
      type(AlquimiaProblemMetaData) :: meta_data
    end subroutine
  end interface
  
  interface
    subroutine AllocateAlquimiaState(sizes, state) bind(C, name='AllocateAlquimiaState')
      use AlquimiaContainers_module, only : AlquimiaSizes, AlquimiaState
      implicit none
      type(AlquimiaSizes) :: sizes
      type(AlquimiaState) :: state
    end subroutine
  end interface
  interface
    subroutine FreeAlquimiaState(state) bind(C, name='FreeAlquimiaState')
      use AlquimiaContainers_module, only : AlquimiaState
      implicit none
      type(AlquimiaState) :: state
    end subroutine
  end interface
  
  interface
    subroutine AllocateAlquimiaProperties(sizes, props) bind(C, name='AllocateAlquimiaProperties')
      use AlquimiaContainers_module, only : AlquimiaSizes, AlquimiaProperties
      implicit none
      type(AlquimiaSizes) :: sizes
      type(AlquimiaProperties) :: props
    end subroutine
  end interface
  interface
    subroutine FreeAlquimiaProperties(props) bind(C, name='FreeAlquimiaProperties')
      use AlquimiaContainers_module, only : AlquimiaProperties
      implicit none
      type(AlquimiaProperties) :: props
    end subroutine
  end interface
  
  interface
    subroutine AllocateAlquimiaAuxiliaryData(sizes, aux_data) bind(C, name='AllocateAlquimiaAuxiliaryData')
      use AlquimiaContainers_module, only : AlquimiaSizes, AlquimiaAuxiliaryData
      implicit none
      type(AlquimiaSizes) :: sizes
      type(AlquimiaAuxiliaryData) :: aux_data
    end subroutine
  end interface
  interface
    subroutine FreeAlquimiaAuxiliaryData(aux_data) bind(C, name='FreeAlquimiaAuxiliaryData')
      use AlquimiaContainers_module, only : AlquimiaAuxiliaryData
      implicit none
      type(AlquimiaAuxiliaryData) :: aux_data
    end subroutine
  end interface
  
  interface
    subroutine AllocateAlquimiaAuxiliaryOutputData(sizes, aux_output) bind(C, name='AllocateAlquimiaAuxiliaryOutputData')
      use AlquimiaContainers_module, only : AlquimiaSizes, AlquimiaAuxiliaryOutputData
      implicit none
      type(AlquimiaSizes) :: sizes
      type(AlquimiaAuxiliaryOutputData) :: aux_output
    end subroutine
  end interface
  interface
    subroutine FreeAlquimiaAuxiliaryOutputData(aux_output) bind(C, name='FreeAlquimiaAuxiliaryOutputData')
      use AlquimiaContainers_module, only : AlquimiaAuxiliaryOutputData
      implicit none
      type(AlquimiaAuxiliaryOutputData) :: aux_output
    end subroutine
  end interface
  
  interface
    subroutine AllocateAlquimiaGeochemicalCondition(size_name, num_aqueous_constraints, num_mineral_constraints,&
                              condition) bind(C, name='AllocateAlquimiaGeochemicalCondition')
      use AlquimiaContainers_module, only : AlquimiaGeochemicalCondition
      use iso_c_binding, only : C_INT
      implicit none
      integer(C_INT),VALUE :: size_name, num_aqueous_constraints, num_mineral_constraints
      type(AlquimiaGeochemicalCondition) :: condition
    end subroutine
  end interface
  interface
    subroutine FreeAlquimiaGeochemicalCondition(condition) bind(C, name='FreeAlquimiaGeochemicalCondition')
      use AlquimiaContainers_module, only : AlquimiaGeochemicalCondition
      implicit none
      type(AlquimiaGeochemicalCondition) :: condition
    end subroutine
  end interface
  
  ! The following subroutines are methods of the engine itself
  
  interface
    subroutine Setup(input_filename,hands_off,pft_engine_state,sizes,functionality,status) bind(C)
      use, intrinsic :: iso_c_binding, only: c_char, c_bool, c_ptr
      use AlquimiaContainers_module, only : AlquimiaEngineStatus,AlquimiaEngineFunctionality,AlquimiaSizes
      IMPORT
      implicit none
      character(kind=c_char) :: input_filename(*)
      logical(c_bool )              :: hands_off
      type(c_ptr) :: pft_engine_state
      type(AlquimiaSizes)     :: sizes
      type(AlquimiaEngineFunctionality)  :: functionality
      type(AlquimiaEngineStatus)  :: status
    end subroutine
  end interface
  
  
    !gracefully shutdown the engine, cleanup memory
    interface
      subroutine Shutdown(pft_engine_state,status) bind(C)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use AlquimiaContainers_module, only : AlquimiaEngineStatus
        
        implicit none
        type(c_ptr) :: pft_engine_state
        type (AlquimiaEngineStatus), intent(out) ::  status
      end subroutine
    end interface
  
    ! constraint processing for boundary/initial constraints. Called
    !   once for each IC/BC.
    interface
      subroutine ProcessCondition(pft_engine_state,condition,props,state,aux_data,status) bind(C)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use AlquimiaContainers_module, only : AlquimiaProperties,&
                 AlquimiaState,AlquimiaAuxiliaryData, AlquimiaEngineStatus,&
                 AlquimiaGeochemicalCondition
        implicit none
        type(c_ptr) :: pft_engine_state
        type(AlquimiaGeochemicalCondition) :: condition
        type(AlquimiaProperties) :: props
        type(AlquimiaState) :: state
        type(AlquimiaAuxiliaryData) :: aux_data
        type(AlquimiaEngineStatus) :: status
        end subroutine
      end interface
  
    ! take one (or more?) reaction steps in operator split mode
    interface
      subroutine ReactionStepOperatorSplit(pft_engine_state, delta_t, props, state, aux_data, status) bind(C)
        use, intrinsic :: iso_c_binding, only : c_ptr, c_double
        use AlquimiaContainers_module, only : AlquimiaSizes,AlquimiaProblemMetaData,AlquimiaProperties,&
                 AlquimiaState,AlquimiaAuxiliaryData,AlquimiaAuxiliaryOutputData, AlquimiaEngineStatus,&
                 AlquimiaGeochemicalCondition,AlquimiaEngineFunctionality
        implicit none
        type(c_ptr) :: pft_engine_state
        real(c_double),VALUE :: delta_t
        type(AlquimiaProperties) :: props
        type(AlquimiaState) :: state
        type(AlquimiaAuxiliaryData) :: aux_data
        type(AlquimiaEngineStatus) :: status
      end subroutine
    end interface
  
    ! Access to user selected geochemical data for output, i.e. pH, 
    !   mineral SI, reaction rates 
    interface
      subroutine GetAuxiliaryOutput(pft_engine_state,props,state,aux_data,aux_out,status) bind(C)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use AlquimiaContainers_module, only : AlquimiaSizes,AlquimiaProblemMetaData,AlquimiaProperties,&
                 AlquimiaState,AlquimiaAuxiliaryData,AlquimiaAuxiliaryOutputData, AlquimiaEngineStatus,&
                 AlquimiaGeochemicalCondition,AlquimiaEngineFunctionality
        IMPORT
        implicit none
        type(c_ptr) :: pft_engine_state
        type(AlquimiaProperties) :: props
        type(AlquimiaState) :: state
        type(AlquimiaAuxiliaryData) :: aux_data
        type(AlquimiaAuxiliaryOutputData) :: aux_out
        type(AlquimiaEngineStatus) :: status
      end subroutine
    end interface
  
    interface
      subroutine GetProblemMetaData(pft_engine_state, meta_data, status) bind(C)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use AlquimiaContainers_module, only : AlquimiaProblemMetaData,AlquimiaEngineStatus
        implicit none
        type(c_ptr) :: pft_engine_state
        type(AlquimiaProblemMetaData) :: meta_data
        type(AlquimiaEngineStatus) :: status
      end subroutine
    end interface

end module alquimia_fortran_interface_mod