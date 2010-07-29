class HomeController < ApplicationController
  def index
    @task = Task.new
    @tasks = Task.all

    respond_to do |format|
      format.html
      format.xml  { render :xml => @posts }
    end
  end

  def create
    @task = Task.new(params[:task])

    respond_to do |format|
      if @task.save
        flash[:notice] = 'Task was successfully created.'
        format.html { redirect_to("home#index") }
        format.xml  { render :xml => @task, :status => :created,
                    :location => @task }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @task.errors,
                    :status => :unprocessable_entity }
      end
    end
  end
end
