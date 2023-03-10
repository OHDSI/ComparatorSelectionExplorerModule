# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of ComparatorSelectionExplorerModule
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

createCseModuleSpecifications <- function(minExposureSize = 1000) {
  analysis <- list()
  for (name in names(formals(createCseModuleSpecifications))) {
    analysis[[name]] <- get(name)
  }
  specifications <- list(module = "ComparatorSelectionExplorerModule",
                         version = "0.0.1",
                         remoteRepo = "github.com",
                         remoteUsername = "ohdsi",
                         settings = analysis)
  class(specifications) <- c("ComparatorSelectionExplorerModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}
