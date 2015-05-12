require 'collins_shell/thor'
require 'collins_shell/util'
require 'thor'
require 'thor/group'

module CollinsShell

  class Ipmi < Thor
    include ThorHelper
    include CollinsShell::Util
    namespace :ipmi
    def self.banner task, namespace = true, subcommand = false
      "#{basename} #{task.formatted_usage(self, true, subcommand).gsub(':',' ')}"
    end

    def self.ipmi_options required = false
      method_option :ipmi_username, :type => :string, :required => required, :desc => 'IPMI username'
      method_option :ipmi_password, :type => :string, :required => required, :desc => 'IPMI password'
      method_option :address, :type => :string, :required => required, :desc => 'IPMI address'
      method_option :gateway, :type => :string, :required => required, :desc => 'IPMI gateway'
      method_option :netmask, :type => :string, :required => required, :desc => 'IPMI netmask'
    end

    def self.print_ipmi ipmi
      puts("address,gateway,netmask,username,password")
      puts([ipmi.address,ipmi.gateway,ipmi.netmask,ipmi.username,ipmi.password].join(','))
    end

    desc 'create', 'create a new IPMI address for the specified asset'
    use_collins_options
    use_tag_option(true)
    ipmi_options(true)
    def create
      call_collins get_collins_client, "create ipmi" do |client|
        ipmi = client.ipmi_create options.tag, options.ipmi_username, options.ipmi_password, options.address, options.gateway, options.netmask
        if ipmi then
          asset = client.get options.tag
          CollinsShell::Ipmi.print_ipmi asset.ipmi
        else
          say_error "create IPMI address"
        end
      end
    end

    desc 'update', 'update IPMI settings for an asset'
    use_collins_options
    use_tag_option(true)
    ipmi_options(false)
    def update
      call_collins get_collins_client, "update ipmi" do |client|
        ipmi = client.ipmi_update options.tag, :username => options.ipmi_username,
                                               :password => options.ipmi_password,
                                               :address => options.address,
                                               :gateway => options.gateway,
                                               :netmask => options.netmask
        if ipmi then
          asset = client.get options.tag
          CollinsShell::Ipmi.print_ipmi asset.ipmi
        else
          say_error "update IPMI address"
        end
      end
    end

  end

end
