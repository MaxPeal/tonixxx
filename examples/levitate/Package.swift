// swift-tools-version:4.0
import PackageDescription

let package = Package(
  name: "Levitate",
  products: [
    .executable(name: "levitate", targets: ["levitate"])
  ],
  dependencies: [],
  targets: [
    .target(name: "levitate", dependencies: [])
  ]
)
