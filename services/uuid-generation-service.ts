import { type IdGenerationService } from './id-generation-service.ts';

export class UuidGenerationService implements IdGenerationService {
  async generateId(): Promise<string> {
    return crypto.randomUUID();
  }
}
