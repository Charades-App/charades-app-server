/// Service used to generate a string ID.
export interface IdGenerationService {
  generateId(): Promise<string>
}
