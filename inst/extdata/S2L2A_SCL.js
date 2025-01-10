//VERSION=3
//Other S2L2A specific data (Aerosol Optical Thickness, Scene Classification, Snow and Cloud probabilities, Sun and View angles)

function setup() {
  return {
    input: [{bands:["SCL"]}],
    output: {
      id: "default",
      bands: 1,
      sampleType: SampleType.INT8
    }
  }
}

function evaluatePixel(sample) {
    return [ sample.SCL]
}
