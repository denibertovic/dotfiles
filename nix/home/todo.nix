{inputs, ...}: {
  imports = [
    inputs.todo.homeManagerModules.todo-sync
  ];

  programs.todo.enable = true;
  services.todo-sync.enable = true;
}
