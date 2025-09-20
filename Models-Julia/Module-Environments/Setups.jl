

# Define a global output directory
const working_directory = "C:/Users/dmayerh/Onedrive - Personal/OneDrive/DATIpilot/Inhaltliches/Papers/Hirschman/SimOutput"
mkpath(working_directory)  # This creates the folder if it doesn't exist#


outputdfRho0giniSPLIT = CSV.read(joinpath(working_directory, "Rho0giniSPLIT2.csv"), DataFrame)
append!(outputdfRho0gini, outputdfRho0giniSPLIT, cols=:union)


outputdfRho0perceptionsGiniSPLIT = CSV.read(joinpath(working_directory, "Rho0perceptionsGiniSPLIT2.csv"), DataFrame)
append!(outputdfRho0perceptionsGini, outputdfRho0perceptionsGiniSPLIT, cols=:union)
