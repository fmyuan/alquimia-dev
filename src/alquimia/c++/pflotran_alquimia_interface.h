/* -*-  mode: c++; c-default-style: "google"; indent-tabs-mode: nil -*- */
#ifndef ALQUIMIA_CXX_PFLOTRAN_INTERFACE_H_
#define ALQUIMIA_CXX_PFLOTRAN_INTERFACE_H_

/*******************************************************************************
 **
 ** C++ implementation of the pflotran alquimia interface
 **
 ******************************************************************************/

#include "alquimia_interface.h"

#include "alquimia_containers.h"

extern "C" {
  void pflotran_alquimia_setup(
      char* input_filename,
      void* pft_engine_state,
      AlquimiaSizes_C* sizes,
      AlquimiaEngineStatus_C* status) ;
  void pflotran_alquimia_shutdown(
      void* pft_engine_state,
      AlquimiaEngineStatus_C* status);
  void pflotran_alquimia_processcondition(
      void* pft_engine_state,
      AlquimiaGeochemicalCondition_C* condition,
      AlquimiaMaterialProperties_C* material_props,
      AlquimiaState_C* state,
      AlquimiaAuxiliaryData_C* aux_data,
      AlquimiaEngineStatus_C* status);
  void pflotran_alquimia_reactionstepoperatorsplit(
      void* pft_engine_state,
      double* delta_t,
      AlquimiaMaterialProperties_C* material_properties,
      AlquimiaState_C* state,
      AlquimiaAuxiliaryData_C* aux_data,
      AlquimiaEngineStatus_C* status);
  void pflotran_alquimia_getauxiliaryoutput(
      void* pft_engine_state,
      AlquimiaEngineStatus_C* status);
  void pflotran_alquimia_getenginemetadata(
      void* pft_engine_state,
      AlquimiaSizes_C* sizes,
      AlquimiaMetaData_C* metadata,
      AlquimiaEngineStatus_C* status);
  void pflotran_alquimia_getprimarynamefromindex(
      void* pft_engine_state,
      int* primary_index,
      char* primary_name,
      AlquimiaEngineStatus_C* status);
}

namespace alquimia {


class PFloTranAlquimiaInterface : public AlquimiaInterface {
 public:
  PFloTranAlquimiaInterface();
  virtual ~PFloTranAlquimiaInterface();

  void Setup(const std::string& input_file,
             AlquimiaSizes_C* sizes,
             AlquimiaEngineStatus_C* status);

  void Shutdown(AlquimiaEngineStatus_C* status);

  void ProcessCondition(AlquimiaGeochemicalCondition_C* condition,
                        AlquimiaMaterialProperties_C* material_props,
                        AlquimiaState_C* state,
                        AlquimiaAuxiliaryData_C* aux_data,
                        AlquimiaEngineStatus_C* status);

  void ReactionStepOperatorSplit(
      double delta_t,
      AlquimiaMaterialProperties_C* material_props,
      AlquimiaState_C* state,
      AlquimiaAuxiliaryData_C* aux_data,
      AlquimiaEngineStatus_C* status);

  void GetAuxiliaryOutput(AlquimiaAuxiliaryData_C* aux_data,
                          AlquimiaEngineStatus_C* status);


  void GetEngineMetaData(AlquimiaSizes_C* sizes,
                         AlquimiaMetaData_C* meta_data,
                         AlquimiaEngineStatus_C* status);

 protected:

 private:
  AlquimiaSizes_C* sizes_;
};

}  // namespace alquimia
#endif  // ALQUIMIA_CXX_PFLOTRAN_INTERFACE_H_
