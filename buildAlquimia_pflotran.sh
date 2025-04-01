
WORK_DIR=${PWD}

echo "PETSC_DIR: $PETSC_DIR"

PFLOTRAN_SRC_DIR=$WORK_DIR/pflotran-elm-interface
ALQUIMIA_SRC_DIR=$WORK_DIR/alquimia-dev

ALQUIMIA_PATH=$1
PFLOTRAN_PATH=$2

## build libpflotranchem.a 
cd $PFLOTRAN_PATH
if [ -d "${PFLOTRAN_PATH}" ]; then
  rm -rf "${PFLOTRAN_PATH}/*"
else
  mkdir -p $PFLOTRAN_PATH
fi
cd $PFLOTRAN_PATH
cp -r $PFLOTRAN_SRC_DIR/src/pflotran/* ./
make PETSC_DIR=$PETSC_DIR clean
make PETSC_DIR=$PETSC_DIR pflotran
make PETSC_DIR=$PETSC_DIR libpflotranchem.a


## ALQUIMIA built with libpflotranchem.a
cd $ALQUIMIA_PATH
if [ -d "${ALQUIMIA_PATH}/build" ]; then
  rm -rf "${ALQUIMIA_PATH}/build"
fi

mkdir -p ${ALQUIMIA_PATH}/build
cd ${ALQUIMIA_PATH}/build

cmake $ALQUIMIA_SRC_DIR/ \
  -DCMAKE_INSTALL_PREFIX=$ALQUIMIA_PATH \
  -DCMAKE_C_COMPILER=`which mpicc` \
  -DCMAKE_CXX_COMPILER=`which mpicxx` \
  -DCMAKE_Fortran_COMPILER=`which mpif90` \
  -DCMAKE_BUILD_TYPE=Debug \
  -DBUILD_SHARED_LIBS=OFF \
  -DXSDK_WITH_PFLOTRAN=ON \
  -DXSDK_WITH_CRUNCHFLOW=OFF \
  -DTPL_PETSC_INCLUDE_DIRS="$PETSC_DIR/include" \
  -DTPL_PETSC_LDFLAGS="-Wl,-rpath,$PETSC_DIR/lib -L$PETSC_DIR/lib -lpetsc" \
  -DTPL_PFLOTRAN_LIBRARIES=$PFLOTRAN_PATH/libpflotranchem.a \
  -DTPL_PFLOTRAN_INCLUDE_DIRS=$PFLOTRAN_PATH \
  -DCMAKE_C_FLAGS="-W -Wall -Wextra -DPFLOTRAN_SOMDEC" \
  -DCMAKE_CXX_FLAGS="-W -Wall -Wextra -DPFLOTRAN_SOMDEC" \
  -DCMAKE_Fortran_FLAGS="-W -Wall -Wextra -DPFLOTRAN_SOMDEC"
make -j 4 VERBOSE=1
make test
make install
# the above 'install' didn't copy *.mod
cp ${ALQUIMIA_PATH}/build/alquimia/*.mod ${ALQUIMIA_PATH}/include/
cp ${ALQUIMIA_PATH}/build/alquimia/*.mod ${ALQUIMIA_PATH}/include/alquimia/
cp ${ALQUIMIA_PATH}/build/alquimia/*.h ${ALQUIMIA_PATH}/include/

cd $WORK_DIR

