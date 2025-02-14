export class Username {
  constructor(public readonly username: string) {
    if (username === "") {
      throw new Error("The username cannot be empty");
    }
  }
}
