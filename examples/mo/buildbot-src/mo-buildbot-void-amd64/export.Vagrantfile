Vagrant.configure("2") do |config|
  config.vm.synced_folder ".", "/vagrant", type: "rsync"

  config.vm.provider "virtualbox" do |vb|
    vb.memory = 4096 # MiB
  end
end
