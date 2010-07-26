class CreateTasks < ActiveRecord::Migration
  def self.up
    create_table :tasks do |t|
      t.string :name
      t.float :expected_time
      t.boolean :complete
      t.float :actual_time

      t.timestamps
    end
  end

  def self.down
    drop_table :tasks
  end
end
